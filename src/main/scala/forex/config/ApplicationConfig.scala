package forex.config

import scala.concurrent.duration.FiniteDuration

case class ApplicationConfig(
    http: HttpConfig,
    oneFrame: OneFrameConfig,
    redis: RedisConfig,
    scheduler: SchedulerConfig,
    system: SystemConfig
)

case class HttpConfig(
    host: String,
    port: Int,
    timeout: FiniteDuration
)

case class OneFrameConfig(
    uri: String,
    token: String,
    quota: QuotaConfig
)

case class RedisConfig(
    host: String,
    port: Int,
    timeout: FiniteDuration,
    ttl: FiniteDuration,
    lockTtl: FiniteDuration,
    lockRetryDelay: FiniteDuration
)

case class SchedulerConfig(
    healthCheckInterval: FiniteDuration,
    backoff: BackoffConfig
)

case class BackoffConfig(
    minBackoffSeconds: Int,
    maxBackoffSeconds: Int,
    multiplier: Int,
    initialBackoffSeconds: Int
)

case class QuotaConfig(
    maxCallsPerDay: Int,
    maxRateAgeSeconds: Int,
    refreshCheckInterval: FiniteDuration
)

case class SystemConfig(
    retryDelay: FiniteDuration
)
