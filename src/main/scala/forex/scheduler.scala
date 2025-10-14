package forex

import cats.effect.{ Sync, Timer }
import cats.effect.concurrent.Ref
import cats.syntax.all._
import forex.config.ApplicationConfig
import forex.domain.{ Currency, Rate }
import forex.programs.rates.Algebra
import forex.services.rates.interpreters.RedisRatesService
import forex.services.quota.QuotaManager
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.DurationDouble

case class BackoffState(
    consecutiveFailures: Int,
    lastFailureTime: Option[Long],
    currentBackoffSeconds: Int,
    config: ApplicationConfig
) {
  def nextBackoff: BackoffState = {
    val nextBackoffSeconds = math.min(
      config.scheduler.backoff.maxBackoffSeconds,
      math.max(
        config.scheduler.backoff.minBackoffSeconds,
        currentBackoffSeconds * config.scheduler.backoff.multiplier
      )
    )
    copy(
      consecutiveFailures = consecutiveFailures + 1,
      lastFailureTime = Some(System.currentTimeMillis()),
      currentBackoffSeconds = nextBackoffSeconds
    )
  }

  def shouldRetry: Boolean =
    lastFailureTime match {
      case None => true
      case Some(lastFailure) =>
        val timeSinceFailure = (System.currentTimeMillis() - lastFailure) / 1000
        timeSinceFailure >= currentBackoffSeconds
    }
}

object scheduler {

  def quotaRefreshScheduler[F[_]: Timer: Logger: Sync](
      config: ApplicationConfig,
      quotaManager: QuotaManager[F]
  ): Stream[F, Unit] = {
    val checkInterval = config.oneFrame.quota.refreshCheckInterval

    Stream.eval(Logger[F].info(s"QUOTA REFRESH SCHEDULER: Starting with ${checkInterval.toSeconds}s check interval")) >>
      Stream
        .repeatEval {
          for {
            refreshed <- quotaManager.checkAndRefreshQuota
            _ <- if (refreshed) {
                  Logger[F].info(s"QUOTA REFRESH: Daily quota has been refreshed successfully")
                } else {
                  Logger[F].info(s"QUOTA REFRESH: No refresh needed yet")
                }
            _ <- Timer[F].sleep(checkInterval)
          } yield ()
        }
  }

  def scheduledUpdateWithBackoff[F[_]: Timer: Logger: Sync](
      config: ApplicationConfig,
      quotaManager: QuotaManager[F],
      oneFrameLiveCaller: Algebra[F],
      redisService: RedisRatesService[F]
  ): Stream[F, Unit] =
    Stream
      .eval(Ref.of[F, BackoffState](BackoffState(0, None, config.scheduler.backoff.initialBackoffSeconds, config)))
      .flatMap { backoffRef =>
        Stream
          .repeatEval {
            for {
              backoffState <- backoffRef.get
              redisHealthy <- checkRedisHealth(redisService)

              _ <- if (redisHealthy) {
                    checkRedisAndCallExternalIfNeeded(
                      config,
                      quotaManager,
                      oneFrameLiveCaller,
                      redisService,
                      backoffRef,
                      backoffState
                    )
                  } else {
                    Logger[F].warn(
                      s"REDIS UNAVAILABLE: Skipping external call to preserve quota (will retry Redis connection)"
                    ) >>
                      Timer[F].sleep(config.scheduler.healthCheckInterval)
                  }
            } yield ()
          }
      }

  private def checkRedisAndCallExternalIfNeeded[F[_]: Timer: Logger: Sync](
      config: ApplicationConfig,
      quotaManager: QuotaManager[F],
      oneFrameLiveCaller: Algebra[F],
      redisService: RedisRatesService[F],
      backoffRef: Ref[F, BackoffState],
      backoffState: BackoffState
  ): F[Unit] =
    for {
      hasFreshData <- redisService.hasFreshData
      cacheAge <- redisService.getCacheDataAge
      quotaStatus <- quotaManager.getQuotaStatus
      _ <- if (!hasFreshData) {
            if (quotaStatus.remainingCalls > 0 && backoffState.shouldRetry) {
              Logger[F].info(s"REDIS EMPTY: No rates found, fetching from external API") >>
                makeTrackedExternalCallWithBackoff(
                  config,
                  quotaManager,
                  oneFrameLiveCaller,
                  redisService,
                  backoffRef
                ) >>
                Timer[F].sleep(5.seconds)
            } else if (quotaStatus.remainingCalls <= 0) {
              Logger[F].warn(s"REDIS EMPTY: No quota remaining, waiting for quota reset") >>
                Timer[F].sleep(60.seconds)
            } else {
              val waitTime = backoffState.currentBackoffSeconds.toDouble.seconds
              Logger[F].info(s"REDIS EMPTY: In backoff period, waiting ${waitTime.toSeconds}s") >>
                Timer[F].sleep(waitTime)
            }
          } else {
            cacheAge match {
              case Some(ageSeconds) if ageSeconds >= config.oneFrame.quota.maxRateAgeSeconds =>
                if (quotaStatus.remainingCalls > 0 && backoffState.shouldRetry) {
                  Logger[F].info(
                    s"REDIS STALE: Data is ${ageSeconds}s old (max ${config.oneFrame.quota.maxRateAgeSeconds}s), refreshing"
                  ) >>
                    makeTrackedExternalCallWithBackoff(
                      config,
                      quotaManager,
                      oneFrameLiveCaller,
                      redisService,
                      backoffRef
                    ) >>
                    Timer[F].sleep(5.seconds)
                } else {
                  Logger[F].warn(s"REDIS STALE: Data needs refresh but quota/backoff prevents it") >>
                    Timer[F].sleep(30.seconds)
                }
              case Some(ageSeconds) =>
                val ttlSeconds  = config.redis.ttl.toSeconds
                val nextCheckIn = math.max(5, ttlSeconds - ageSeconds)
                Logger[F].info(
                  s"REDIS FRESH: Data is ${ageSeconds}s old (TTL: ${ttlSeconds}s), next check in ${nextCheckIn}s"
                ) >>
                  Timer[F].sleep(nextCheckIn.toDouble.seconds)
              case None =>
                Timer[F].sleep(5.seconds)
            }
          }
    } yield ()

  private def checkRedisHealth[F[_]: Logger: Sync](redisService: RedisRatesService[F]): F[Boolean] = {
    import forex.domain.Currency._
    import cats.data.NonEmptyList

    val testPair = NonEmptyList.one(Rate.Pair(USD, EUR))

    redisService
      .getAll(testPair)
      .map {
        case Right(_) => true
        case Left(error) =>
          val errorMsg = error.toString
          if (errorMsg.contains("No rates found in cache")) {
            true
          } else {
            false
          }
      }
      .handleErrorWith { ex =>
        Logger[F].debug(s"Redis health check failed: ${ex.getMessage}") >>
          false.pure[F]
      }
  }

  private def makeTrackedExternalCallWithBackoff[F[_]: Logger: Sync](
      config: ApplicationConfig,
      quotaManager: QuotaManager[F],
      oneFrameLiveCaller: Algebra[F],
      redisService: RedisRatesService[F],
      backoffRef: Ref[F, BackoffState]
  ): F[Unit] =
    oneFrameLiveCaller.allRates
      .chunkN(Currency.allCombinationsLength)
      .evalMap { rates =>
        val ratesList = rates.toList
        for {
          _ <- Logger[F].info(s"EXTERNAL API SUCCESS: Fetched ${ratesList.length} rates")
          _ <- quotaManager.recordSuccessfulCall
          _ <- backoffRef.set(BackoffState(0, None, config.scheduler.backoff.initialBackoffSeconds, config))
          result <- redisService.saveRatesClusterSafe(ratesList).attempt
          _ <- result match {
                case Right(_) =>
                  Logger[F].info(s"REDIS SUCCESS: Saved ${ratesList.length} rates to cache")
                case Left(redisError) =>
                  Logger[F].error(s" REDIS ERROR: Failed to save rates: ${redisError.toString}")
              }
        } yield ()
      }
      .compile
      .drain
      .handleErrorWith { externalError =>
        for {
          _ <- quotaManager.recordFailedCall
          _ <- Logger[F].warn(s"EXTERNAL API FAILURE: ${externalError.toString}")
          currentState <- backoffRef.get
          newState = currentState.nextBackoff
          _ <- backoffRef.set(newState)
          _ <- Logger[F].info(
                s"BACKOFF UPDATED: Next retry in ${newState.currentBackoffSeconds}s (failures: ${newState.consecutiveFailures})"
              )
        } yield ()
      }
}
