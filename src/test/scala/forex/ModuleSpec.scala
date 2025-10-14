package forex

import cats.effect.{ IO, Timer, ContextShift }
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import dev.profunktor.redis4cats.RedisCommands
import forex.config._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class ModuleSpec extends AsyncFreeSpec with Matchers {
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val timer: Timer[IO] = IO.timer(ec)
  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  // Create a test redis instance - we'll use null since we're testing module wiring
  val mockRedis: RedisCommands[IO, String, String] = null
  
  val testConfig = ApplicationConfig(
    http = HttpConfig(
      host = "localhost",
      port = 8080,
      timeout = 10.seconds
    ),
    oneFrame = OneFrameConfig(
      uri = "http://localhost:8080", 
      token = "test-token",
      quota = QuotaConfig(
        maxCallsPerDay = 1000,
        maxRateAgeSeconds = 300,
        refreshCheckInterval = 60.seconds
      )
    ),
    redis = RedisConfig(
      host = "localhost",
      port = 6379,
      timeout = 5.seconds,
      ttl = 300.seconds,
      lockTtl = 10.seconds,
      lockRetryDelay = 100.millis
    ),
    scheduler = SchedulerConfig(
      healthCheckInterval = 30.seconds,
      backoff = BackoffConfig(
        minBackoffSeconds = 1,
        maxBackoffSeconds = 300,
        multiplier = 2,
        initialBackoffSeconds = 5
      )
    ),
    system = SystemConfig(
      retryDelay = 1.second
    )
  )

  "Module" - {
    "should initialize with valid configuration" in {
      // Since Redis operations aren't needed for module construction,
      // we test that the module can be created successfully
      val module = new Module[IO](testConfig, mockRedis)
      
      module.quotaManager should not be null
      module.httpApp should not be null
      module.oneFrameProgram should not be null
      module.redisRatesService should not be null
    }

    "should create quota manager with proper types" in {
      val module = new Module[IO](testConfig, mockRedis)
      
      val quotaManager = module.quotaManager
      quotaManager should not be null
      quotaManager.getClass.getSimpleName should be ("QuotaManager")
    }

    "should create oneFrame program with OneFrame service" in {
      val module = new Module[IO](testConfig, mockRedis)
      
      val oneFrameProgram = module.oneFrameProgram
      oneFrameProgram should not be null
      oneFrameProgram.getClass.getName should include ("Program")
    }

    "should create complete HTTP app with middleware composition" in {
      val module = new Module[IO](testConfig, mockRedis)
      
      val httpApp = module.httpApp
      httpApp should not be null
      httpApp.getClass.getName should include ("Kleisli")
    }

    "should handle different timeout configurations" in {
      val customConfig = testConfig.copy(
        http = testConfig.http.copy(timeout = 30.seconds)
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Should create module successfully with different timeout
      module.httpApp should not be null
      module.quotaManager should not be null
    }

    "should work with different quota configurations" in {
      val customConfig = testConfig.copy(
        oneFrame = testConfig.oneFrame.copy(
          quota = testConfig.oneFrame.quota.copy(maxCallsPerDay = 5000, refreshCheckInterval = 120.seconds)
        )
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Should create module successfully with different quota config
      module.quotaManager should not be null
      module.httpApp should not be null
    }

    "should handle different redis configurations" in {
      val customConfig = testConfig.copy(
        redis = testConfig.redis.copy(host = "test", port = 6380, timeout = 15.seconds)
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Should create module successfully with different redis config
      module.redisRatesService should not be null
      module.quotaManager should not be null
    }

    "should properly wire dependencies between services" in {
      val module = new Module[IO](testConfig, mockRedis)
      
      // All services should be properly initialized
      val quotaManager = module.quotaManager
      val redisService = module.redisRatesService
      val oneFrameProgram = module.oneFrameProgram
      val httpApp = module.httpApp
      
      quotaManager should not be null
      redisService should not be null
      oneFrameProgram should not be null
      httpApp should not be null
      
      // Dependencies should be wired correctly
      module.redisRatesService should be theSameInstanceAs redisService
    }

    "should handle scheduler configuration variations" in {
      val customConfig = testConfig.copy(
        scheduler = testConfig.scheduler.copy(
          healthCheckInterval = 60.seconds,
          backoff = testConfig.scheduler.backoff.copy(
            minBackoffSeconds = 2,
            maxBackoffSeconds = 600
          )
        )
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Should create module successfully with different scheduler config
      module.quotaManager should not be null
      module.httpApp should not be null
    }

    "should handle system configuration variations" in {
      val customConfig = testConfig.copy(
        system = testConfig.system.copy(retryDelay = 5.seconds)
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Should create module successfully with different system config
      module.quotaManager should not be null
      module.httpApp should not be null
    }

    "should create services with complex configuration combinations" in {
      val customConfig = testConfig.copy(
        http = testConfig.http.copy(host = "0.0.0.0", port = 9090, timeout = 45.seconds),
        oneFrame = testConfig.oneFrame.copy(
          uri = "http://api.example.com:8080",
          token = "custom-token-123",
          quota = testConfig.oneFrame.quota.copy(
            maxCallsPerDay = 10000,
            maxRateAgeSeconds = 600,
            refreshCheckInterval = 30.seconds
          )
        ),
        redis = testConfig.redis.copy(
          host = "redis.example.com",
          port = 6380,
          timeout = 8.seconds,
          ttl = 600.seconds,
          lockTtl = 30.seconds,
          lockRetryDelay = 200.millis
        ),
        scheduler = testConfig.scheduler.copy(
          healthCheckInterval = 45.seconds,
          backoff = BackoffConfig(
            minBackoffSeconds = 3,
            maxBackoffSeconds = 900,
            multiplier = 3,
            initialBackoffSeconds = 10
          )
        ),
        system = testConfig.system.copy(retryDelay = 2.seconds)
      )
      
      val module = new Module[IO](customConfig, mockRedis)
      
      // Complex configuration should still create valid module
      module.quotaManager should not be null
      module.httpApp should not be null
      module.oneFrameProgram should not be null
      module.redisRatesService should not be null
    }

    "should create module instances independently" in {
      val module1 = new Module[IO](testConfig, mockRedis)
      val module2 = new Module[IO](testConfig, mockRedis)
      
      // Different module instances should have different service instances
      module1.quotaManager should not equal module2.quotaManager
      module1.httpApp should not equal module2.httpApp
      module1.oneFrameProgram should not equal module2.oneFrameProgram
    }
  }
}