package forex.http.health

import cats.effect.Sync
import cats.syntax.all._
import forex.domain.Rate
import forex.services.quota.QuotaManager
import forex.services.rates.interpreters.RedisRatesService
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityEncoder._

import java.time.Instant
case class ServiceHealth(
    service: String,
    status: String,
    message: String,
    lastChecked: String
)

case class QuotaHealth(
    remainingCalls: Int,
    totalCalls: Int,
    successRate: Double,
    resetTime: String,
    lastCallTime: Option[String],
    status: String,
    message: String
)

case class CacheHealth(
    status: String,
    message: String,
    rateCount: Int,
    oldestRateAge: Option[String]
)

case class OverallHealth(
    status: String,
    timestamp: String,
    services: List[ServiceHealth],
    quota: QuotaHealth,
    cache: CacheHealth,
    canProvideRates: Boolean,
    issues: List[String]
)

class HealthHttpRoutes[F[_]: Sync: Logger](
    quotaManager: QuotaManager[F],
    redisService: RedisRatesService[F]
) extends Http4sDsl[F] {

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "health" =>
      Logger[F].info(s"HEALTH CHECK: Comprehensive health check requested") >>
        performHealthCheck
          .flatMap { health =>
            val httpStatus = if (health.canProvideRates) {
              if (health.status == "HEALTHY") Ok(health) else Ok(health)
            } else {
              ServiceUnavailable(health)
            }
            Logger[F].info(s"HEALTH RESULT: ${health.status} - Can provide rates: ${health.canProvideRates}") >>
              httpStatus
          }
          .handleErrorWith { error =>
            Logger[F].error(s"HEALTH CHECK ERROR: ${error.getMessage}") >>
              InternalServerError(
                OverallHealth(
                  status = "ERROR",
                  timestamp = Instant.now().toString,
                  services = List.empty,
                  quota = QuotaHealth(0, 0, 0.0, "", None, "ERROR", "Health check failed"),
                  cache = CacheHealth("ERROR", "Health check failed", 0, None),
                  canProvideRates = false,
                  issues = List(s"Health check system error: ${error.getMessage}")
                )
              )
          }

    case GET -> Root / "health" / "simple" =>
      Logger[F].info(s"SIMPLE HEALTH CHECK: Simple health status requested") >>
        performHealthCheck
          .flatMap { health =>
            case class SimpleHealthResponse(status: String, canProvideRates: Boolean)
            val status = SimpleHealthResponse(health.status, health.canProvideRates)
            val httpStatus = if (health.canProvideRates) {
              Ok(status)
            } else {
              ServiceUnavailable(status)
            }
            Logger[F].info(s"SIMPLE RESULT: Status=${health.status}, CanProvideRates=${health.canProvideRates}") >>
              httpStatus
          }
          .handleErrorWith { error =>
            case class ErrorResponse(status: String, message: String)
            Logger[F].error(s"Simple health check failed: ${error.getMessage}") >>
              InternalServerError(ErrorResponse("ERROR", "Health check failed"))
          }

    case GET -> Root / "ready" =>
      Logger[F].info(s"READINESS CHECK: Checking if service is ready to serve requests") >>
        checkReadiness.flatMap { isReady =>
          case class ReadinessResponse(status: String, timestamp: String)
          val status   = if (isReady) "READY" else "NOT_READY"
          val response = ReadinessResponse(status, Instant.now().toString)
          Logger[F].info(s"READINESS RESULT: $status") >>
            (if (isReady) Ok(response) else ServiceUnavailable(response))
        }

    case POST -> Root / "admin" / "quota" / "refresh" =>
      Logger[F].info(s"ADMIN REQUEST: Manual quota refresh requested") >>
        quotaManager.forceQuotaRefresh
          .flatMap { _ =>
            case class QuotaRefreshResponse(status: String, message: String, timestamp: String)
            val response = QuotaRefreshResponse(
              "SUCCESS",
              "Quota has been manually refreshed",
              Instant.now().toString
            )
            Logger[F].info(s"ADMIN QUOTA REFRESH: Quota manually refreshed successfully") >>
              Ok(response)
          }
          .handleErrorWith { error =>
            case class ErrorResponse(status: String, error: String, timestamp: String)
            Logger[F].error(s"ADMIN QUOTA REFRESH ERROR: ${error.getMessage}") >>
              InternalServerError(
                ErrorResponse(
                  "ERROR",
                  s"Failed to refresh quota: ${error.getMessage}",
                  Instant.now().toString
                )
              )
          }
  }

  private def performHealthCheck: F[OverallHealth] =
    for {
      quotaStatus <- quotaManager.getQuotaStatus
      cacheHealth <- checkCacheHealth
      canProvideRates = determineCanProvideRates(quotaStatus, cacheHealth)
      issues          = calculateIssues(quotaStatus, cacheHealth)
      overallStatus   = if (issues.isEmpty) "HEALTHY" else if (canProvideRates) "DEGRADED" else "UNHEALTHY"
    } yield
      OverallHealth(
        status = overallStatus,
        timestamp = Instant.now().toString,
        services = List(
          ServiceHealth(
            "external-api",
            getExternalApiStatus(quotaStatus, canProvideRates),
            getExternalApiMessage(quotaStatus, canProvideRates),
            Instant.now().toString
          ),
          ServiceHealth(
            "redis-cache",
            getRedisServiceStatus(cacheHealth),
            getRedisServiceMessage(cacheHealth),
            Instant.now().toString
          )
        ),
        quota = QuotaHealth(
          remainingCalls = quotaStatus.remainingCalls,
          totalCalls = quotaStatus.totalCalls,
          successRate = quotaStatus.successRate,
          resetTime = quotaStatus.resetTime.toString,
          lastCallTime = quotaStatus.lastCallTime.map(_.toString),
          status =
            if (quotaStatus.remainingCalls > 100) "HEALTHY"
            else if (quotaStatus.remainingCalls > 0) "LOW"
            else "EXHAUSTED",
          message = getQuotaMessage(quotaStatus)
        ),
        cache = cacheHealth,
        canProvideRates = canProvideRates,
        issues = issues
      )

  private def performSimpleHealthCheck: F[Boolean] =
    for {
      quotaStatus <- quotaManager.getQuotaStatus
      cacheHealth <- checkCacheHealth
    } yield quotaStatus.remainingCalls > 0 || cacheHealth.rateCount > 0

  private def checkReadiness: F[Boolean] =
    performSimpleHealthCheck

  private def checkCacheHealth: F[CacheHealth] = {
    import forex.domain.Currency._
    import cats.data.NonEmptyList

    val testPairs = NonEmptyList.of(
      Rate.Pair(USD, EUR),
      Rate.Pair(USD, GBP),
      Rate.Pair(EUR, JPY)
    )

    Logger[F].debug(s"HEALTH: Testing Redis connection with ${testPairs.length} pairs") >>
      redisService
        .getAll(testPairs)
        .flatMap {
          case Right(rates) =>
            val rateCount  = rates.length
            val oldestRate = rates.toList.minByOption(_.timestamp.value)
            val oldestAge = oldestRate.map { rate =>
              val nowInstant  = Instant.now()
              val rateInstant = rate.timestamp.value.toInstant
              val ageSeconds  = (nowInstant.toEpochMilli - rateInstant.toEpochMilli) / 1000
              if (ageSeconds < 60) s"${ageSeconds}s"
              else if (ageSeconds < 3600) s"${ageSeconds / 60}m"
              else s"${ageSeconds / 3600}h"
            }

            val status =
              if (rateCount >= testPairs.length) "HEALTHY"
              else if (rateCount > 0) "PARTIAL"
              else "EMPTY"

            val message =
              if (rateCount >= testPairs.length) s"Cache healthy with ${rateCount} fresh rates"
              else if (rateCount > 0) s"Partial cache with ${rateCount}/${testPairs.length} rates"
              else "Cache is empty but Redis is connected"

            Logger[F].debug(s"REDIS HEALTH SUCCESS: Status=$status, Count=$rateCount") >>
              CacheHealth(status, message, rateCount, oldestAge).pure[F]

          case Left(error) =>
            val errorMsg = error.toString
            if (errorMsg.contains("No rates found in cache")) {
              Logger[F].debug(s"REDIS HEALTH: Cache is empty but connected") >>
                CacheHealth("EMPTY", "Cache is empty but Redis is connected", 0, None).pure[F]
            } else {
              Logger[F].warn(s"REDIS HEALTH ERROR: $error") >>
                CacheHealth("DOWN", "Unable to connect to Redis cache", 0, None).pure[F]
            }
        }
        .handleErrorWith { error =>
          Logger[F].error(s"REDIS HEALTH EXCEPTION: ${error.getMessage}") >>
            CacheHealth("ERROR", "Cache health check failed", 0, None).pure[F]
        }
  }

  private def calculateIssues(quotaStatus: forex.services.quota.QuotaStatus, cacheHealth: CacheHealth): List[String] = {
    var issues = List.empty[String]

    if (quotaStatus.remainingCalls <= 0) {
      issues = "API quota exhausted - cannot make external calls" :: issues
    } else if (quotaStatus.remainingCalls < 50) {
      issues = s"API quota running low: ${quotaStatus.remainingCalls} calls remaining" :: issues
    }

    if (cacheHealth.status == "DOWN" || cacheHealth.status == "ERROR") {
      issues = "Redis cache is not accessible" :: issues
    } else if (cacheHealth.status == "EMPTY") {
      issues = "No cached rates available (Redis connected but empty)" :: issues
    } else if (cacheHealth.status == "PARTIAL") {
      issues = "Cache is incomplete - some rates missing" :: issues
    }

    cacheHealth.oldestRateAge match {
      case Some(age) if age.contains("h") =>
        issues = s"Some cached rates are very old ($age)" :: issues
      case Some(age) if age.contains("m") && age.replace("m", "").toIntOption.exists(_ > 10) =>
        issues = s"Some cached rates are getting stale ($age)" :: issues
      case _ =>
    }

    issues.reverse
  }

  private def determineCanProvideRates(quotaStatus: forex.services.quota.QuotaStatus,
                                       cacheHealth: CacheHealth): Boolean = {
    val hasValidCache = cacheHealth.rateCount > 0 && (cacheHealth.status == "HEALTHY" || cacheHealth.status == "PARTIAL")
    val hasPotentialExternalApi = quotaStatus.remainingCalls > 0 && (
      quotaStatus.totalCalls == 0 ||
      quotaStatus.totalCalls < 5 ||
      quotaStatus.successRate > 0.05
    )

    hasValidCache || hasPotentialExternalApi
  }

  private def getRedisServiceStatus(cacheHealth: CacheHealth): String =
    cacheHealth.status match {
      case "DOWN" | "ERROR"                => "DOWN"
      case "HEALTHY" | "PARTIAL" | "EMPTY" => "UP"
      case _                               => "UNKNOWN"
    }

  private def getRedisServiceMessage(cacheHealth: CacheHealth): String =
    cacheHealth.status match {
      case "DOWN"    => "Unable to connect to Redis cache"
      case "ERROR"   => "Redis cache error"
      case "HEALTHY" => cacheHealth.message
      case "PARTIAL" => cacheHealth.message
      case "EMPTY"   => "Redis connected but no rates cached"
      case _         => cacheHealth.message
    }

  private def getExternalApiStatus(quotaStatus: forex.services.quota.QuotaStatus, canProvideRates: Boolean): String =
    if (canProvideRates) {
      "UP"
    } else if (quotaStatus.remainingCalls > 0) {
      "DEGRADED"
    } else {
      "DOWN"
    }

  private def getExternalApiMessage(quotaStatus: forex.services.quota.QuotaStatus, canProvideRates: Boolean): String =
    if (canProvideRates) {
      s"Service operational - providing live rates (${quotaStatus.remainingCalls} quota remaining)"
    } else if (quotaStatus.remainingCalls > 0) {
      "Service issues detected - unable to provide rates despite available quota"
    } else {
      s"Quota exhausted - resets at ${quotaStatus.resetTime}"
    }

  private def getQuotaMessage(quotaStatus: forex.services.quota.QuotaStatus): String =
    if (quotaStatus.remainingCalls <= 0) {
      s"Quota exhausted. Resets at ${quotaStatus.resetTime}"
    } else if (quotaStatus.remainingCalls < 50) {
      s"Quota running low: ${quotaStatus.remainingCalls} calls remaining"
    } else {
      s"Quota healthy: ${quotaStatus.remainingCalls} calls remaining"
    }

  val routes: HttpRoutes[F] = Router("/forex" -> httpRoutes)
}
