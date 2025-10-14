package forex.services.quota

import cats.effect.{ Clock, Sync }
import cats.syntax.all._
import dev.profunktor.redis4cats.RedisCommands
import forex.config.ApplicationConfig
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._
import java.time.{ Instant, LocalDate, ZoneOffset }
import java.util.concurrent.TimeUnit

case class QuotaStatus(
    remainingCalls: Int,
    totalCalls: Int,
    resetTime: Instant,
    successRate: Double,
    lastCallTime: Option[Instant]
)

case class SchedulingDecision(
    shouldCall: Boolean,
    nextCallIn: FiniteDuration,
    reason: String
)

class QuotaManager[F[_]: Sync: Logger: Clock](
    redis: RedisCommands[F, String, String],
    config: ApplicationConfig
) {

  private val maxCallsPerDay    = config.oneFrame.quota.maxCallsPerDay
  private val maxRateAgeSeconds = config.oneFrame.quota.maxRateAgeSeconds

  private val quotaKeyPrefix    = "forex:quota:"
  private val remainingCallsKey = s"${quotaKeyPrefix}remaining"
  private val totalCallsKey     = s"${quotaKeyPrefix}total"
  private val successCountKey   = s"${quotaKeyPrefix}success"
  private val failureCountKey   = s"${quotaKeyPrefix}failure"
  private val lastCallTimeKey   = s"${quotaKeyPrefix}last_call"
  private val resetTimeKey      = s"${quotaKeyPrefix}reset_time"

  def getQuotaStatus: F[QuotaStatus] =
    for {
      remaining <- redis.get(remainingCallsKey).map(_.map(_.toInt).getOrElse(maxCallsPerDay))
      total <- redis.get(totalCallsKey).map(_.map(_.toInt).getOrElse(0))
      success <- redis.get(successCountKey).map(_.map(_.toInt).getOrElse(0))
      lastCall <- redis.get(lastCallTimeKey).map(_.map(Instant.parse))
      resetTime <- getOrSetResetTime
      successRate = if (total > 0) success.toDouble / total else 1.0
    } yield QuotaStatus(remaining, total, resetTime, successRate, lastCall)

  def recordSuccessfulCall: F[Unit] =
    for {
      now <- Clock[F].realTime(TimeUnit.MILLISECONDS).map(Instant.ofEpochMilli)
      _ <- Logger[F].info(s"QUOTA: Recording successful API call")
      remaining <- redis.get(remainingCallsKey).map(_.map(_.toInt).getOrElse(maxCallsPerDay))
      _ <- redis.set(remainingCallsKey, (remaining - 1).toString)
      total <- redis.get(totalCallsKey).map(_.map(_.toInt).getOrElse(0))
      _ <- redis.set(totalCallsKey, (total + 1).toString)
      success <- redis.get(successCountKey).map(_.map(_.toInt).getOrElse(0))
      _ <- redis.set(successCountKey, (success + 1).toString)
      _ <- redis.set(lastCallTimeKey, now.toString)
      status <- getQuotaStatus
      _ <- Logger[F].info(
            s"QUOTA STATUS: ${status.remainingCalls}/${maxCallsPerDay} calls remaining, success rate: ${(status.successRate * 100).toInt}%"
          )
    } yield ()

  def recordFailedCall: F[Unit] =
    for {
      now <- Clock[F].realTime(TimeUnit.MILLISECONDS).map(Instant.ofEpochMilli)
      _ <- Logger[F].warn(s"QUOTA: Recording FAILED API call (quota NOT consumed)")
      total <- redis.get(totalCallsKey).map(_.map(_.toInt).getOrElse(0))
      _ <- redis.set(totalCallsKey, (total + 1).toString)
      failure <- redis.get(failureCountKey).map(_.map(_.toInt).getOrElse(0))
      _ <- redis.set(failureCountKey, (failure + 1).toString)
      _ <- redis.set(lastCallTimeKey, now.toString)
      status <- getQuotaStatus
      _ <- Logger[F].warn(
            s"QUOTA STATUS: ${status.remainingCalls}/${maxCallsPerDay} calls remaining, success rate: ${(status.successRate * 100).toInt}%"
          )
    } yield ()

  def calculateOptimalSchedule: F[SchedulingDecision] =
    for {
      status <- getQuotaStatus
      now <- Clock[F].realTime(TimeUnit.MILLISECONDS).map(Instant.ofEpochMilli)
      decision <- makeSchedulingDecision(status, now)
      _ <- Logger[F].info(s"SCHEDULING DECISION: ${decision.reason}")
    } yield decision

  private def makeSchedulingDecision(status: QuotaStatus, now: Instant): F[SchedulingDecision] = {
    val secondsUntilReset = (status.resetTime.toEpochMilli - now.toEpochMilli) / 1000
    val hoursUntilReset   = secondsUntilReset / 3600.0

    val rateAge = status.lastCallTime
      .map { lastCall =>
        (now.toEpochMilli - lastCall.toEpochMilli) / 1000
      }
      .getOrElse(Long.MaxValue)

    if (rateAge >= maxRateAgeSeconds) {
      SchedulingDecision(
        shouldCall = true,
        nextCallIn = 0.seconds,
        reason = s"URGENT: Rates are ${rateAge}s old (max ${maxRateAgeSeconds}s). Must refresh immediately."
      ).pure[F]
    } else if (status.remainingCalls <= 0) {
      val nextCall = (status.resetTime.toEpochMilli - now.toEpochMilli).millis
      SchedulingDecision(
        shouldCall = false,
        nextCallIn = nextCall,
        reason = s"Quota exhausted. Waiting ${nextCall.toMinutes} minutes for reset."
      ).pure[F]
    } else if (hoursUntilReset <= 0) {
      resetQuota.map { _ =>
        SchedulingDecision(
          shouldCall = true,
          nextCallIn = 0.seconds,
          reason = "Quota reset complete. Making immediate call."
        )
      }
    } else {
      val minCallsNeeded          = (24 * 60) / (maxRateAgeSeconds / 60)
      val safeCallsRemaining      = (status.remainingCalls * status.successRate).toInt
      val optimalFrequencyHours   = hoursUntilReset / math.max(safeCallsRemaining, minCallsNeeded)
      val optimalFrequencySeconds = (optimalFrequencyHours * 3600).toInt

      // Use Redis TTL as minimum interval to avoid calling external API while cache is still fresh
      val redisTtlSeconds = config.redis.ttl.toSeconds.toInt
      val actualInterval  = math.max(optimalFrequencySeconds, redisTtlSeconds)

      val shouldCallNow = status.lastCallTime match {
        case Some(lastCall) =>
          val timeSinceLastCall = (now.toEpochMilli - lastCall.toEpochMilli) / 1000
          timeSinceLastCall >= actualInterval
        case None => true
      }

      if (shouldCallNow) {
        SchedulingDecision(
          shouldCall = true,
          nextCallIn = 0.seconds,
          reason =
            s"Data refresh needed: ${safeCallsRemaining} effective calls for ${hoursUntilReset.toInt}h (${(status.successRate * 100).toInt}% success rate)"
        ).pure[F]
      } else {
        val waitTime = status.lastCallTime
          .map { lastCall =>
            val timeSinceLastCall = (now.toEpochMilli - lastCall.toEpochMilli) / 1000
            (actualInterval - timeSinceLastCall).seconds
          }
          .getOrElse(0.seconds)

        SchedulingDecision(
          shouldCall = false,
          nextCallIn = waitTime,
          reason = s"Redis cache fresh: waiting ${waitTime.toSeconds}s (TTL-based interval: ${actualInterval}s)"
        ).pure[F]
      }
    }
  }

  private def getOrSetResetTime: F[Instant] =
    redis.get(resetTimeKey).flatMap {
      case Some(timeStr) =>
        val resetTime = Instant.parse(timeStr)
        Clock[F].realTime(TimeUnit.MILLISECONDS).map(Instant.ofEpochMilli).flatMap { now =>
          if (now.isAfter(resetTime) || now.equals(resetTime)) {
            val nextReset = LocalDate.now(ZoneOffset.UTC).plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
            redis.set(resetTimeKey, nextReset.toString) *> nextReset.pure[F]
          } else {
            resetTime.pure[F]
          }
        }
      case None =>
        val tomorrow = LocalDate.now(ZoneOffset.UTC).plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        redis.set(resetTimeKey, tomorrow.toString) *> tomorrow.pure[F]
    }

  private def resetQuota: F[Unit] =
    for {
      _ <- Logger[F].info(s"QUOTA RESET: Resetting daily quota to ${maxCallsPerDay} calls")
      tomorrow <- {
        val nextReset = LocalDate.now(ZoneOffset.UTC).plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        nextReset.pure[F]
      }
      _ <- redis.set(remainingCallsKey, maxCallsPerDay.toString)
      _ <- redis.set(totalCallsKey, "0")
      _ <- redis.set(successCountKey, "0")
      _ <- redis.set(failureCountKey, "0")
      _ <- redis.set(resetTimeKey, tomorrow.toString)
      _ <- Logger[F].info(s"QUOTA RESET: Complete. Next reset at ${tomorrow}")
    } yield ()

  def forceQuotaRefresh: F[Unit] =
    for {
      _ <- Logger[F].info(s"MANUAL REFRESH: Forcing quota refresh")
      _ <- resetQuota
    } yield ()

  def checkAndRefreshQuota: F[Boolean] =
    for {
      now <- Clock[F].realTime(TimeUnit.MILLISECONDS).map(Instant.ofEpochMilli)
      resetTime <- getOrSetResetTime
      needsRefresh = now.isAfter(resetTime) || now.equals(resetTime)
      refreshed <- if (needsRefresh) {
                    Logger[F].info(s"AUTO REFRESH: Daily quota reset time reached, refreshing quota") *>
                      resetQuota.as(true)
                  } else {
                    false.pure[F]
                  }
    } yield refreshed

  def getQuotaInfo: F[String] =
    getQuotaStatus.map { status =>
      val hoursUntilReset = {
        val now   = java.time.Instant.now()
        val hours = (status.resetTime.toEpochMilli - now.toEpochMilli) / (1000 * 3600)
        math.max(0, hours)
      }

      s"""Quota Status:
         |  Remaining: ${status.remainingCalls}/${maxCallsPerDay}
         |  Success Rate: ${(status.successRate * 100).toInt}%
         |  Reset in: ${hoursUntilReset}h
         |  Last Call: ${status.lastCallTime.map(_.toString).getOrElse("Never")}""".stripMargin
    }
}
