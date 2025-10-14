package forex.services.rates.interpreters

import cats.data.NonEmptyList
import cats.effect.{ Clock, Sync, Timer }
import cats.syntax.all._
import dev.profunktor.redis4cats.RedisCommands
import forex.config.ApplicationConfig
import forex.domain.Rate
import forex.domain.JsonCodecs._
import forex.services.rates.Algebra
import forex.services.rates.errors.Error
import io.circe.syntax._
import io.circe.parser.decode
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit

class RedisRatesService[F[_]: Sync: Timer: Logger: Clock](
    redis: RedisCommands[F, String, String],
    config: ApplicationConfig
) extends Algebra[F] {
  private val keyPrefix      = "forex:rate:"
  private val lockPrefix     = "forex:lock:"
  private val ttl            = config.redis.ttl
  private val lockTtl        = config.redis.lockTtl
  private val lockRetryDelay = config.redis.lockRetryDelay

  private def rateKey(pair: Rate.Pair): String = s"$keyPrefix${pair.from.show}:${pair.to.show}"
  private def allRatesLockKey: String          = s"${lockPrefix}all_rates"
  private def nodeId: F[String]                = Clock[F].realTime(TimeUnit.MILLISECONDS).map(_.toString)

  override def get(pair: Rate.Pair): F[Error Either Rate] = {
    val key = rateKey(pair)
    redis
      .get(key)
      .flatMap {
        case Some(rateJson) =>
          (decode[Rate](rateJson) match {
            case Right(rate) =>
              Logger[F].info(s"REDIS SUCCESS: rate=${rate.pair.show} price=${rate.price}") >>
                (rate.asRight[Error]: Either[Error, Rate]).pure[F]
            case Left(parseError) =>
              Logger[F].error(s"PARSE ERROR: $parseError") >>
                (Error.RateLookupFailed("Failed to parse cached rate").asLeft[Rate]: Either[Error, Rate]).pure[F]
          })
        case None =>
          Logger[F].warn(s"REDIS MISS: No data for key=$key") >>
            (Error.RateLookupFailed(s"Rate not found for pair: ${pair.show}").asLeft[Rate]: Either[Error, Rate])
              .pure[F]
      }
      .handleErrorWith { ex =>
        Logger[F].error(ex)(s"REDIS ERROR: GET failed for key=$key") >>
          (Error.RateLookupFailed(s"Redis error: ${ex.getMessage}").asLeft[Rate]: Either[Error, Rate]).pure[F]
      }
  }

  override def getAll(pairs: NonEmptyList[Rate.Pair]): F[Error Either NonEmptyList[Rate]] = {
    val keys = pairs.map(rateKey)
    redis
      .mGet(keys.toList.toSet)
      .flatMap { resultMap =>
        Logger[F].info(s"REDIS RESPONSE: ${resultMap.size} entries").flatMap { _ =>
          val rates = pairs.toList.flatMap { pair =>
            val key = rateKey(pair)
            resultMap.get(key).flatMap { rateJson =>
              decode[Rate](rateJson).toOption
            }
          }

          NonEmptyList.fromList(rates) match {
            case Some(ratesNel) =>
              (ratesNel.asRight[Error]: Either[Error, NonEmptyList[Rate]]).pure[F]
            case None =>
              Logger[F].warn(s"REDIS EMPTY: No rates found") >>
                (Error
                  .RateLookupFailed("No rates found in cache")
                  .asLeft[NonEmptyList[Rate]]: Either[Error, NonEmptyList[Rate]]).pure[F]
          }
        }
      }
      .handleErrorWith { ex =>
        Logger[F].error(ex)(s"REDIS ERROR: MGET failed") >>
          (Error
            .RateLookupFailed(s"Redis error: ${ex.getMessage}")
            .asLeft[NonEmptyList[Rate]]: Either[Error, NonEmptyList[Rate]]).pure[F]
      }
  }

  private def acquireLock(lockKey: String): F[Boolean] =
    nodeId
      .flatMap { id =>
        redis.setNx(lockKey, id).flatMap {
          case true =>
            redis.expire(lockKey, lockTtl) *>
              Logger[F].debug(s"Acquired lock: $lockKey with id: $id") *>
              true.pure[F]
          case false =>
            Logger[F].debug(s"Failed to acquire lock: $lockKey") *>
              false.pure[F]
        }
      }
      .handleErrorWith { ex =>
        Logger[F].error(ex)(s"Error acquiring lock: $lockKey") *>
          false.pure[F]
      }

  private def releaseLock(lockKey: String): F[Unit] =
    nodeId
      .flatMap { id =>
        redis.get(lockKey).flatMap {
          case Some(lockValue) if lockValue == id =>
            redis.del(lockKey).void
          case _ =>
            Sync[F].unit
        }
      }
      .handleErrorWith { ex =>
        Logger[F].error(ex)(s"Error releasing lock: $lockKey")
      }

  private def ratesExistInRedis: F[Boolean] =
    redis.keys(s"${keyPrefix}*").map(_.nonEmpty).handleErrorWith { _ =>
      false.pure[F]
    }

  def saveRate(rate: Rate): F[Unit] = {
    val key      = rateKey(rate.pair)
    val rateJson = rate.asJson.noSpaces

    redis.setEx(key, rateJson, ttl).void.handleErrorWith { ex =>
      Logger[F].error(ex)(s"Failed to save rate to Redis for pair: ${rate.pair.show}")
    }
  }

  def saveRatesClusterSafe(rates: List[Rate]): F[Unit] =
    ratesExistInRedis.flatMap { exist =>
      if (exist) {
        saveRatesInternal(rates)
      } else {
        acquireLock(allRatesLockKey).flatMap { lockAcquired =>
          if (lockAcquired) {
            Logger[F].info("Acquired cluster lock, saving rates to Redis") *>
              saveRatesInternal(rates).flatMap { _ =>
                releaseLock(allRatesLockKey)
              }
          } else {
            Logger[F].info("Another node is fetching rates, waiting...") *>
              Timer[F].sleep(lockRetryDelay) *>
              ratesExistInRedis.flatMap { nowExist =>
                if (nowExist) {
                  Logger[F].info("Rates now available in Redis")
                } else {
                  Logger[F].warn("Rates still not available after waiting")
                }
              }
          }
        }
      }
    }

  private def saveRatesInternal(rates: List[Rate]): F[Unit] = {
    val keyValuePairs = rates.map(rate => rateKey(rate.pair) -> rate.asJson.noSpaces).toMap

    redis
      .mSet(keyValuePairs)
      .flatMap { _ =>
        rates.traverse_ { rate =>
          val key      = rateKey(rate.pair)
          val rateJson = rate.asJson.noSpaces
          redis.setEx(key, rateJson, ttl)
        }
      }
      .handleErrorWith { ex =>
        Logger[F].error(ex)("Failed to save rates to Redis")
      }
  }

  def saveRates(rates: List[Rate]): F[Unit] = saveRatesInternal(rates)

  def hasFreshData: F[Boolean] = {
    import forex.domain.Currency._
    val criticalPairs = List(
      Rate.Pair(USD, EUR),
      Rate.Pair(GBP, USD),
      Rate.Pair(EUR, GBP)
    )

    val keys = criticalPairs.map(rateKey)

    redis
      .mGet(keys.toSet)
      .map { resultMap =>
        val hasData = resultMap.values.count(_.nonEmpty) >= criticalPairs.length / 2
        hasData
      }
      .handleErrorWith { _ =>
        false.pure[F]
      }
  }

  def getCacheDataAge: F[Option[Long]] = {
    import forex.domain.Currency._
    val testPair = Rate.Pair(USD, EUR)
    val key      = rateKey(testPair)

    redis
      .get(key)
      .flatMap {
        case Some(rateJson) =>
          decode[Rate](rateJson) match {
            case Right(rate) =>
              Clock[F].realTime(TimeUnit.SECONDS).map { now =>
                val rateAge = now - rate.timestamp.value.toEpochSecond
                (Some(rateAge): Option[Long])
              }
            case Left(_) =>
              Sync[F].pure(None: Option[Long])
          }
        case None =>
          Sync[F].pure(None: Option[Long])
      }
      .handleErrorWith { _ =>
        Sync[F].pure(None: Option[Long])
      }
  }
}
