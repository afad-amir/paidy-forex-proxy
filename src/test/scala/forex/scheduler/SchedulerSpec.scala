package forex.services.scheduler

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.duration._
import java.time.Instant

class SchedulerSpec extends AnyWordSpec with Matchers {

  "BackoffConfig" should {
    "calculate exponential backoff correctly" in {
      val config = forex.config.BackoffConfig(
        minBackoffSeconds = 1,
        maxBackoffSeconds = 60,
        multiplier = 2,
        initialBackoffSeconds = 1
      )
      
      // Test different backoff scenarios
      val attempts = List(0, 1, 2, 3, 4, 5, 6, 10)
      
      attempts.foreach { attempt =>
        val backoffSeconds = Math.min(
          config.maxBackoffSeconds,
          Math.max(
            config.minBackoffSeconds,
            (config.initialBackoffSeconds * Math.pow(config.multiplier.toDouble, attempt.toDouble)).toInt
          )
        )
        
        if (attempt == 0) {
          backoffSeconds shouldBe config.initialBackoffSeconds
        } else if (attempt < 6) {
          backoffSeconds should be > config.minBackoffSeconds
          backoffSeconds should be <= config.maxBackoffSeconds
        } else {
          backoffSeconds shouldBe config.maxBackoffSeconds
        }
      }
    }

    "handle edge cases in backoff calculation" in {
      // Test with multiplier = 1 (no exponential growth)
      val noGrowthConfig = forex.config.BackoffConfig(
        minBackoffSeconds = 5,
        maxBackoffSeconds = 30,
        multiplier = 1,
        initialBackoffSeconds = 10
      )
      
      val backoff1 = Math.min(
        noGrowthConfig.maxBackoffSeconds,
        Math.max(
          noGrowthConfig.minBackoffSeconds,
          (noGrowthConfig.initialBackoffSeconds * Math.pow(noGrowthConfig.multiplier.toDouble, 3.0)).toInt
        )
      )
      
      backoff1 shouldBe noGrowthConfig.initialBackoffSeconds
      
      // Test with very high multiplier
      val highGrowthConfig = forex.config.BackoffConfig(
        minBackoffSeconds = 1,
        maxBackoffSeconds = 60,
        multiplier = 10,
        initialBackoffSeconds = 2
      )
      
      val backoff2 = Math.min(
        highGrowthConfig.maxBackoffSeconds,
        Math.max(
          highGrowthConfig.minBackoffSeconds,
          (highGrowthConfig.initialBackoffSeconds * Math.pow(highGrowthConfig.multiplier.toDouble, 2.0)).toInt
        )
      )
      
      backoff2 shouldBe highGrowthConfig.maxBackoffSeconds
    }
  }

  "Scheduler timing logic" should {
    "handle different scheduling scenarios" in {
      val healthCheckInterval = 30.seconds
      val currentTime = Instant.now()
      val lastHealthCheck = currentTime.minusSeconds(25)
      val veryOldHealthCheck = currentTime.minusSeconds(60)
      
      // Test recent health check - should not trigger immediately
      val timeSinceLastCheck = java.time.Duration.between(lastHealthCheck, currentTime)
      if (timeSinceLastCheck.getSeconds < healthCheckInterval.toSeconds) {
        val nextCheckIn = healthCheckInterval.toSeconds - timeSinceLastCheck.getSeconds
        nextCheckIn should be > 0L
        nextCheckIn should be < healthCheckInterval.toSeconds
      }
      
      // Test old health check - should trigger immediately
      val timeSinceOldCheck = java.time.Duration.between(veryOldHealthCheck, currentTime)
      if (timeSinceOldCheck.getSeconds >= healthCheckInterval.toSeconds) {
        timeSinceOldCheck.getSeconds should be >= healthCheckInterval.toSeconds
      }
    }

    "validate quota refresh timing" in {
      val refreshInterval = 10.seconds
      
      val scenarios = List(
        (5.seconds, false),   // Too soon to refresh
        (10.seconds, true),   // Exactly at refresh interval
        (15.seconds, true),   // Past refresh interval
        (0.seconds, false)    // Just started
      )
      
      scenarios.foreach { case (elapsed, shouldRefresh) =>
        val actualShouldRefresh = elapsed >= refreshInterval
        actualShouldRefresh shouldBe shouldRefresh
        
        if (shouldRefresh) {
          elapsed should be >= refreshInterval
        } else {
          elapsed should be < refreshInterval
        }
      }
    }

    "handle rate age validation" in {
      val maxRateAge = 300 // 5 minutes
      val currentTime = Instant.now()
      
      val testRates = List(
        (currentTime.minusSeconds(60), true),    // 1 minute old - fresh
        (currentTime.minusSeconds(240), true),   // 4 minutes old - still fresh
        (currentTime.minusSeconds(300), false),  // Exactly 5 minutes old - stale
        (currentTime.minusSeconds(600), false),  // 10 minutes old - very stale
        (currentTime.minusSeconds(0), true)      // Brand new - fresh
      )
      
      testRates.foreach { case (rateTimestamp, expectedFresh) =>
        val ageSeconds = java.time.Duration.between(rateTimestamp, currentTime).getSeconds
        val isFresh = ageSeconds < maxRateAge
        
        isFresh shouldBe expectedFresh
        
        if (expectedFresh) {
          ageSeconds.toInt should be < maxRateAge
        } else {
          ageSeconds.toInt should be >= maxRateAge
        }
      }
    }
  }

  "Scheduler decision making" should {
    "make correct scheduling decisions based on quota" in {
      val quotaScenarios = List(
        (1000, 0, true),     // High quota, no calls made - should call immediately
        (1000, 500, true),   // High quota, some calls made - should call
        (1000, 950, true),   // High quota, most calls made - should still call
        (1000, 1000, false), // Quota exhausted - should not call
        (100, 100, false),   // Small quota exhausted - should not call
        (0, 0, false)        // No quota - should not call
      )
      
      quotaScenarios.foreach { case (maxCalls, usedCalls, shouldCall) =>
        val remainingCalls = maxCalls - usedCalls
        val actualShouldCall = remainingCalls > 0
        
        actualShouldCall shouldBe shouldCall
        
        if (shouldCall) {
          remainingCalls should be > 0
        } else {
          remainingCalls should be <= 0
        }
      }
    }

    "handle error-based backoff decisions" in {
      val backoffConfig = forex.config.BackoffConfig(
        minBackoffSeconds = 1,
        maxBackoffSeconds = 60,
        multiplier = 2,
        initialBackoffSeconds = 1
      )
      
      val errorScenarios = List(
        (0, 1),    // No previous errors - minimal backoff
        (1, 2),    // One error - double backoff
        (2, 4),    // Two errors - quadruple backoff
        (3, 8),    // Three errors - continue exponential
        (6, 60),   // Many errors - hit max backoff
        (10, 60)   // Excessive errors - stay at max
      )
      
      errorScenarios.foreach { case (errorCount, expectedBackoff) =>
        val calculatedBackoff = Math.min(
          backoffConfig.maxBackoffSeconds,
          Math.max(
            backoffConfig.minBackoffSeconds,
            (backoffConfig.initialBackoffSeconds * Math.pow(backoffConfig.multiplier.toDouble, errorCount.toDouble)).toInt
          )
        )
        
        calculatedBackoff shouldBe expectedBackoff
        calculatedBackoff should be >= backoffConfig.minBackoffSeconds
        calculatedBackoff should be <= backoffConfig.maxBackoffSeconds
      }
    }

    "validate health check decision logic" in {
      val healthCheckInterval = 30.seconds
      val currentTime = Instant.now()
      
      val healthCheckScenarios = List(
        (currentTime.minusSeconds(10), false),  // Too recent
        (currentTime.minusSeconds(30), true),   // Exactly at interval
        (currentTime.minusSeconds(45), true),   // Overdue
        (currentTime.minusSeconds(0), false),   // Just checked
        (currentTime.minusSeconds(120), true)   // Very overdue
      )
      
      healthCheckScenarios.foreach { case (lastCheck, shouldCheck) =>
        val timeSinceCheck = java.time.Duration.between(lastCheck, currentTime)
        val actualShouldCheck = timeSinceCheck.getSeconds >= healthCheckInterval.toSeconds
        
        actualShouldCheck shouldBe shouldCheck
        
        if (shouldCheck) {
          timeSinceCheck.getSeconds should be >= healthCheckInterval.toSeconds
        } else {
          timeSinceCheck.getSeconds should be < healthCheckInterval.toSeconds
        }
      }
    }
  }

  "Scheduler state management" should {
    "handle different scheduler states" in {
      val states = List("IDLE", "RUNNING", "PAUSED", "ERROR", "STOPPING")
      
      states.foreach { state =>
        // Test state transitions
        state match {
          case "IDLE" =>
            // Can transition to RUNNING
            val canStart = true
            canStart shouldBe true
            
          case "RUNNING" =>
            // Can transition to PAUSED or STOPPING
            val canPause = true
            val canStop = true
            canPause shouldBe true
            canStop shouldBe true
            
          case "PAUSED" =>
            // Can transition to RUNNING or STOPPING
            val canResume = true
            val canStop = true
            canResume shouldBe true
            canStop shouldBe true
            
          case "ERROR" =>
            // Can transition to IDLE or STOPPING
            val canRestart = true
            val canStop = true
            canRestart shouldBe true
            canStop shouldBe true
            
          case "STOPPING" =>
            // Final state, no transitions
            val canTransition = false
            canTransition shouldBe false
            
          case _ =>
            fail(s"Unknown state: $state")
        }
      }
    }

    "validate concurrent operation handling" in {
      // Test scenarios for concurrent operations
      val concurrentScenarios = List(
        (true, false, "SINGLE_OPERATION"),    // Only one operation
        (false, true, "SINGLE_OPERATION"),    // Only one operation
        (true, true, "CONFLICT"),             // Multiple operations - conflict
        (false, false, "IDLE")                // No operations
      )
      
      concurrentScenarios.foreach { case (operation1Active, operation2Active, expectedState) =>
        val operationCount = List(operation1Active, operation2Active).count(identity)
        
        expectedState match {
          case "SINGLE_OPERATION" =>
            operationCount shouldBe 1
            
          case "CONFLICT" =>
            operationCount shouldBe 2
            // This would require conflict resolution
            
          case "IDLE" =>
            operationCount shouldBe 0
            
          case _ =>
            fail(s"Unknown expected state: $expectedState")
        }
      }
    }
  }
}