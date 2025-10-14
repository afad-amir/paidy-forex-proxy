package forex.services.quota

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.duration._
import forex.config._

class QuotaManagerSpec extends AnyWordSpec with Matchers {

  def createTestConfig(): ApplicationConfig = {
    ApplicationConfig(
      HttpConfig("localhost", 8080, 30.seconds),
      OneFrameConfig(
        "https://test.com", 
        "test-token",
        QuotaConfig(1000, 300, 10.seconds)
      ),
      RedisConfig("localhost", 6379, 5.seconds, 1.hour, 30.seconds, 1.second),
      SchedulerConfig(30.seconds, BackoffConfig(1, 60, 2, 1)),
      SystemConfig(5.seconds)
    )
  }

  "QuotaStatus" should {
    "be properly constructed" in {
      val status = QuotaStatus(
        remainingCalls = 100,
        totalCalls = 1000,
        resetTime = java.time.Instant.now(),
        successRate = 95.5,
        lastCallTime = Some(java.time.Instant.now())
      )
      
      status.remainingCalls shouldBe 100
      status.totalCalls shouldBe 1000
      status.successRate shouldBe 95.5
      status.lastCallTime shouldBe defined
    }

    "handle edge cases" in {
      val status = QuotaStatus(
        remainingCalls = 0,
        totalCalls = 1000,
        resetTime = java.time.Instant.now(),
        successRate = 0.0,
        lastCallTime = None
      )
      
      status.remainingCalls shouldBe 0
      status.successRate shouldBe 0.0
      status.lastCallTime shouldBe None
    }
  }

  "SchedulingDecision" should {
    "be properly constructed for immediate action" in {
      val decision = SchedulingDecision(
        shouldCall = true,
        nextCallIn = 0.seconds,
        reason = "Quota available"
      )
      
      decision.shouldCall shouldBe true
      decision.nextCallIn shouldBe 0.seconds
      decision.reason shouldBe "Quota available"
    }

    "be properly constructed for delayed action" in {
      val decision = SchedulingDecision(
        shouldCall = false,
        nextCallIn = 1.hour,
        reason = "Quota exhausted"
      )
      
      decision.shouldCall shouldBe false
      decision.nextCallIn shouldBe 1.hour
      decision.reason shouldBe "Quota exhausted"
    }
  }

  "ApplicationConfig" should {
    "create valid test configuration" in {
      val config = createTestConfig()
      
      config.http.host shouldBe "localhost"
      config.http.port shouldBe 8080
      config.oneFrame.quota.maxCallsPerDay shouldBe 1000
      config.redis.host shouldBe "localhost"
      config.scheduler.healthCheckInterval shouldBe 30.seconds
    }
  }
}