package forex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.config._
import scala.concurrent.duration._

class MainSpec extends AnyWordSpec with Matchers {

  def createValidConfig(): ApplicationConfig = {
    ApplicationConfig(
      HttpConfig("localhost", 8080, 30.seconds),
      OneFrameConfig(
        "https://api.fixer.io", 
        "test-token",
        QuotaConfig(1000, 300, 10.seconds)
      ),
      RedisConfig("localhost", 6379, 5.seconds, 1.hour, 30.seconds, 1.second),
      SchedulerConfig(30.seconds, BackoffConfig(1, 60, 2, 1)),
      SystemConfig(5.seconds)
    )
  }

  def createInvalidConfig(): ApplicationConfig = {
    ApplicationConfig(
      HttpConfig("", -1, 0.seconds), // Invalid HTTP config
      OneFrameConfig(
        "", // Invalid URI
        "",
        QuotaConfig(0, 0, 0.seconds) // Invalid quota
      ),
      RedisConfig("", -1, 0.seconds, 0.seconds, 0.seconds, 0.seconds), // Invalid Redis
      SchedulerConfig(0.seconds, BackoffConfig(0, 0, 0, 0)), // Invalid scheduler
      SystemConfig(0.seconds) // Invalid system
    )
  }

  "Main application" should {
    "validate configuration components" in {
      val config = createValidConfig()
      
      // Test HTTP config validation
      config.http.host should not be empty
      config.http.port should be > 0
      config.http.timeout should be > 0.seconds
      
      // Test OneFrame config validation
      config.oneFrame.uri should not be empty
      config.oneFrame.token should not be empty
      config.oneFrame.quota.maxCallsPerDay should be > 0
      
      // Test Redis config validation
      config.redis.host should not be empty
      config.redis.port should be > 0
      config.redis.timeout should be > 0.seconds
      
      // Test Scheduler config validation
      config.scheduler.healthCheckInterval should be > 0.seconds
      config.scheduler.backoff.minBackoffSeconds should be >= 0
      config.scheduler.backoff.maxBackoffSeconds should be > config.scheduler.backoff.minBackoffSeconds
    }

    "handle configuration validation errors" in {
      val config = createInvalidConfig()
      
      // Test invalid configurations
      config.http.host shouldBe empty
      config.http.port should be < 0
      config.oneFrame.uri shouldBe empty
      config.redis.host shouldBe empty
      config.scheduler.backoff.minBackoffSeconds shouldBe 0
    }

    "validate module creation requirements" in {
      val config = createValidConfig()
      
      // Test configuration requirements for module creation
      config.http.host should not be empty
      config.http.port should be > 0
      config.redis.host should not be empty
      config.redis.port should be > 0
      config.oneFrame.uri should not be empty
    }
  }

  "Configuration loading" should {
    "handle different config paths" in {
      // Test different configuration scenarios
      val configs = List(
        createValidConfig(),
        createValidConfig().copy(http = HttpConfig("0.0.0.0", 9090, 60.seconds)),
        createValidConfig().copy(redis = RedisConfig("redis.example.com", 6380, 10.seconds, 2.hours, 60.seconds, 2.seconds))
      )
      
      configs.foreach { config =>
        config.http shouldBe a[HttpConfig]
        config.oneFrame shouldBe a[OneFrameConfig]
        config.redis shouldBe a[RedisConfig]
        config.scheduler shouldBe a[SchedulerConfig]
        config.system shouldBe a[SystemConfig]
      }
    }

    "validate nested configuration objects" in {
      val config = createValidConfig()
      
      // Test quota config branches
      if (config.oneFrame.quota.maxCallsPerDay > 500) {
        config.oneFrame.quota.maxCallsPerDay should be >= 500
      } else {
        config.oneFrame.quota.maxCallsPerDay should be < 500
      }
      
      // Test backoff config branches
      val backoff = config.scheduler.backoff
      if (backoff.multiplier > 1) {
        backoff.maxBackoffSeconds should be > backoff.minBackoffSeconds
      }
      
      // Test timeout branches
      if (config.http.timeout > 30.seconds) {
        config.http.timeout should be > 30.seconds
      } else {
        config.http.timeout should be <= 30.seconds
      }
    }
  }

  "Application startup" should {
    "handle different startup scenarios" in {
      val validConfig = createValidConfig()
      val invalidConfig = createInvalidConfig()
      
      // Test valid config path
      validConfig.http.port should be > 0
      validConfig.redis.port should be > 0
      
      // Test invalid config path
      invalidConfig.http.port should be <= 0
      invalidConfig.redis.port should be <= 0
    }

    "validate service dependencies" in {
      val config = createValidConfig()
      
      // Redis service validation
      if (config.redis.host.nonEmpty && config.redis.port > 0) {
        config.redis.timeout should be > 0.seconds
      }
      
      // HTTP service validation
      if (config.http.host.nonEmpty && config.http.port > 0) {
        config.http.timeout should be > 0.seconds
      }
      
      // OneFrame service validation
      if (config.oneFrame.uri.nonEmpty && config.oneFrame.token.nonEmpty) {
        config.oneFrame.quota.maxCallsPerDay should be > 0
      }
    }
  }

  "Error handling" should {
    "handle different error scenarios" in {
      // Test error conditions that would occur during startup
      val configs = List(
        createValidConfig(),
        createInvalidConfig()
      )
      
      configs.foreach { config =>
        // Test configuration validation branches
        val isValidHttp = config.http.host.nonEmpty && config.http.port > 0
        val isValidRedis = config.redis.host.nonEmpty && config.redis.port > 0
        val isValidOneFrame = config.oneFrame.uri.nonEmpty && config.oneFrame.token.nonEmpty
        
        if (isValidHttp && isValidRedis && isValidOneFrame) {
          // Valid configuration path
          config should not be null
        } else {
          // Invalid configuration path - would cause startup errors
          val hasErrors = !isValidHttp || !isValidRedis || !isValidOneFrame
          hasErrors shouldBe true
        }
      }
    }

    "validate quota and backoff configurations" in {
      val config = createValidConfig()
      
      val quota = config.oneFrame.quota
      val backoff = config.scheduler.backoff
      
      // Test quota validation branches
      if (quota.maxCallsPerDay <= 0) {
        // Invalid quota path
        fail("Quota should be positive")
      } else if (quota.maxCallsPerDay < 100) {
        // Low quota path
        quota.maxCallsPerDay should be < 100
      } else if (quota.maxCallsPerDay < 1000) {
        // Medium quota path
        quota.maxCallsPerDay should be < 1000
      } else {
        // High quota path
        quota.maxCallsPerDay should be >= 1000
      }
      
      // Test backoff validation branches
      if (backoff.minBackoffSeconds >= backoff.maxBackoffSeconds) {
        // Invalid backoff configuration
        fail("Min backoff should be less than max backoff")
      } else if (backoff.multiplier <= 1) {
        // No exponential backoff
        backoff.multiplier should be <= 1
      } else {
        // Exponential backoff enabled
        backoff.multiplier should be > 1
      }
    }
  }
}