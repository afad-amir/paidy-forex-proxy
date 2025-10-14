package forex.config

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.duration._

class ConfigSpec extends AnyWordSpec with Matchers {

  "HttpConfig" should {
    "be properly constructed" in {
      val config = HttpConfig("localhost", 8080, 30.seconds)
      
      config.host shouldBe "localhost"
      config.port shouldBe 8080
      config.timeout shouldBe 30.seconds
    }

    "handle different hosts and ports" in {
      val config = HttpConfig("0.0.0.0", 9090, 60.seconds)
      
      config.host shouldBe "0.0.0.0"
      config.port shouldBe 9090
      config.timeout shouldBe 60.seconds
    }
  }

  "OneFrameConfig" should {
    "be properly constructed" in {
      val quotaConfig = QuotaConfig(1000, 300, 10.seconds)
      val config = OneFrameConfig("https://api.example.com", "token123", quotaConfig)
      
      config.uri shouldBe "https://api.example.com"
      config.token shouldBe "token123"
      config.quota shouldBe quotaConfig
    }

    "contain valid quota configuration" in {
      val quotaConfig = QuotaConfig(500, 600, 5.seconds)
      val config = OneFrameConfig("https://test.com", "test-token", quotaConfig)
      
      config.quota.maxCallsPerDay shouldBe 500
      config.quota.maxRateAgeSeconds shouldBe 600
      config.quota.refreshCheckInterval shouldBe 5.seconds
    }
  }

  "RedisConfig" should {
    "be properly constructed" in {
      val config = RedisConfig(
        host = "localhost",
        port = 6379,
        timeout = 5.seconds,
        ttl = 1.hour,
        lockTtl = 30.seconds,
        lockRetryDelay = 1.second
      )
      
      config.host shouldBe "localhost"
      config.port shouldBe 6379
      config.timeout shouldBe 5.seconds
      config.ttl shouldBe 1.hour
      config.lockTtl shouldBe 30.seconds
      config.lockRetryDelay shouldBe 1.second
    }

    "handle different redis configurations" in {
      val config = RedisConfig(
        host = "redis.example.com",
        port = 6380,
        timeout = 10.seconds,
        ttl = 2.hours,
        lockTtl = 60.seconds,
        lockRetryDelay = 2.seconds
      )
      
      config.host shouldBe "redis.example.com"
      config.port shouldBe 6380
      config.ttl shouldBe 2.hours
    }
  }

  "SchedulerConfig" should {
    "be properly constructed" in {
      val backoffConfig = BackoffConfig(1, 60, 2, 1)
      val config = SchedulerConfig(30.seconds, backoffConfig)
      
      config.healthCheckInterval shouldBe 30.seconds
      config.backoff shouldBe backoffConfig
    }

    "contain valid backoff configuration" in {
      val backoffConfig = BackoffConfig(5, 120, 3, 2)
      val config = SchedulerConfig(45.seconds, backoffConfig)
      
      config.backoff.minBackoffSeconds shouldBe 5
      config.backoff.maxBackoffSeconds shouldBe 120
      config.backoff.multiplier shouldBe 3
      config.backoff.initialBackoffSeconds shouldBe 2
    }
  }

  "QuotaConfig" should {
    "be properly constructed" in {
      val config = QuotaConfig(1000, 300, 10.seconds)
      
      config.maxCallsPerDay shouldBe 1000
      config.maxRateAgeSeconds shouldBe 300
      config.refreshCheckInterval shouldBe 10.seconds
    }

    "handle different quota limits" in {
      val config = QuotaConfig(2000, 600, 15.seconds)
      
      config.maxCallsPerDay shouldBe 2000
      config.maxRateAgeSeconds shouldBe 600
      config.refreshCheckInterval shouldBe 15.seconds
    }
  }

  "BackoffConfig" should {
    "be properly constructed" in {
      val config = BackoffConfig(1, 60, 2, 1)
      
      config.minBackoffSeconds shouldBe 1
      config.maxBackoffSeconds shouldBe 60
      config.multiplier shouldBe 2
      config.initialBackoffSeconds shouldBe 1
    }

    "handle exponential backoff parameters" in {
      val config = BackoffConfig(5, 300, 3, 10)
      
      config.minBackoffSeconds shouldBe 5
      config.maxBackoffSeconds shouldBe 300
      config.multiplier shouldBe 3
      config.initialBackoffSeconds shouldBe 10
    }
  }

  "SystemConfig" should {
    "be properly constructed" in {
      val config = SystemConfig(5.seconds)
      
      config.retryDelay shouldBe 5.seconds
    }

    "handle different retry delays" in {
      val config = SystemConfig(10.seconds)
      
      config.retryDelay shouldBe 10.seconds
    }
  }

  "ApplicationConfig" should {
    "be properly constructed with all components" in {
      val httpConfig = HttpConfig("localhost", 8080, 30.seconds)
      val quotaConfig = QuotaConfig(1000, 300, 10.seconds)
      val oneFrameConfig = OneFrameConfig("https://api.example.com", "token123", quotaConfig)
      val redisConfig = RedisConfig("localhost", 6379, 5.seconds, 1.hour, 30.seconds, 1.second)
      val backoffConfig = BackoffConfig(1, 60, 2, 1)
      val schedulerConfig = SchedulerConfig(30.seconds, backoffConfig)
      val systemConfig = SystemConfig(5.seconds)
      
      val appConfig = ApplicationConfig(
        http = httpConfig,
        oneFrame = oneFrameConfig,
        redis = redisConfig,
        scheduler = schedulerConfig,
        system = systemConfig
      )
      
      appConfig.http shouldBe httpConfig
      appConfig.oneFrame shouldBe oneFrameConfig
      appConfig.redis shouldBe redisConfig
      appConfig.scheduler shouldBe schedulerConfig
      appConfig.system shouldBe systemConfig
    }

    "provide access to nested configuration values" in {
      val quotaConfig = QuotaConfig(2000, 600, 15.seconds)
      val oneFrameConfig = OneFrameConfig("https://test.com", "test-token", quotaConfig)
      val httpConfig = HttpConfig("0.0.0.0", 9090, 60.seconds)
      val redisConfig = RedisConfig("redis.test.com", 6380, 10.seconds, 2.hours, 60.seconds, 2.seconds)
      val backoffConfig = BackoffConfig(5, 300, 3, 10)
      val schedulerConfig = SchedulerConfig(45.seconds, backoffConfig)
      val systemConfig = SystemConfig(10.seconds)
      
      val appConfig = ApplicationConfig(httpConfig, oneFrameConfig, redisConfig, schedulerConfig, systemConfig)
      
      appConfig.http.port shouldBe 9090
      appConfig.oneFrame.quota.maxCallsPerDay shouldBe 2000
      appConfig.redis.ttl shouldBe 2.hours
      appConfig.scheduler.backoff.multiplier shouldBe 3
      appConfig.system.retryDelay shouldBe 10.seconds
    }
  }
}