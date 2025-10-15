package forex

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import forex.config._

class BackoffStateSpec extends AnyWordSpec with Matchers {

  // Create a test configuration
  def createTestConfig(
    minBackoff: Int = 1,
    maxBackoff: Int = 60,
    multiplier: Int = 2
  ): ApplicationConfig = {
    import scala.concurrent.duration._
    ApplicationConfig(
      HttpConfig("localhost", 8080, 5.seconds),
      OneFrameConfig("http://test", "test-token", QuotaConfig(1000, 3600, 60.seconds)),
      RedisConfig("localhost", 6379, 5.seconds, 300.seconds, 30.seconds, 100.millis),
      SchedulerConfig(60.seconds, BackoffConfig(minBackoff, maxBackoff, multiplier, 1)),
      SystemConfig(1.second)
    )
  }

  "BackoffState comprehensive testing for branch coverage" should {

    "test initial state and shouldRetry logic" in {
      val config = createTestConfig()
      val initialState = BackoffState(0, None, 1, config)
      
      initialState.consecutiveFailures shouldBe 0
      initialState.lastFailureTime shouldBe None
      initialState.currentBackoffSeconds shouldBe 1
      
      // Test shouldRetry conditional logic - no previous failure
      initialState.shouldRetry shouldBe true
    }

    "test shouldRetry conditional logic with timing" in {
      val config = createTestConfig()
      val currentTime = System.currentTimeMillis()
      
      // Test recent failure - should not retry
      val recentFailureState = BackoffState(1, Some(currentTime - 500), 2, config) // 0.5 seconds ago, 2 second backoff
      recentFailureState.shouldRetry shouldBe false
      
      // Test old failure - should retry
      val oldFailureState = BackoffState(1, Some(currentTime - 5000), 2, config) // 5 seconds ago, 2 second backoff
      oldFailureState.shouldRetry shouldBe true
      
      // Test exact boundary condition
      val boundaryState = BackoffState(1, Some(currentTime - 2000), 2, config) // Exactly 2 seconds ago
      boundaryState.shouldRetry shouldBe true // Should be >= backoff time
    }

    "test nextBackoff exponential progression and capping logic" in {
      val config = createTestConfig(minBackoff = 1, maxBackoff = 16, multiplier = 2)
      var state = BackoffState(0, None, 1, config)
      
      // Test exponential progression
      val expectedProgression = List(1, 2, 4, 8, 16, 16, 16) // Should cap at 16
      val actualProgression = scala.collection.mutable.ListBuffer[Int]()
      
      for (i <- expectedProgression.indices) {
        actualProgression += state.currentBackoffSeconds
        state = state.nextBackoff
        
        // Test that consecutive failures increase
        state.consecutiveFailures shouldBe (i + 1)
        
        // Test that lastFailureTime is set
        state.lastFailureTime shouldBe defined
      }
      
      actualProgression.toList shouldBe expectedProgression
      
      // Test that it caps at maxBackoff
      state.currentBackoffSeconds shouldBe config.scheduler.backoff.maxBackoffSeconds
    }

    "test minBackoff boundary condition" in {
      val config = createTestConfig(minBackoff = 5, maxBackoff = 20, multiplier = 2)
      
      // Start with backoff less than min
      val lowState = BackoffState(1, None, 3, config)
      val nextState = lowState.nextBackoff
      
      // Should respect minBackoff
      nextState.currentBackoffSeconds should be >= config.scheduler.backoff.minBackoffSeconds
    }

    "test maxBackoff boundary condition" in {
      val config = createTestConfig(minBackoff = 1, maxBackoff = 10, multiplier = 3)
      
      // Start with high backoff that would exceed max when multiplied
      val highState = BackoffState(5, None, 8, config)
      val nextState = highState.nextBackoff
      
      // Should cap at maxBackoff
      nextState.currentBackoffSeconds shouldBe config.scheduler.backoff.maxBackoffSeconds
    }

    "test different multiplier values and conditional capping" in {
      val multipliers = List(2, 3, 5, 10)
      
      multipliers.foreach { multiplier =>
        val config = createTestConfig(minBackoff = 1, maxBackoff = 100, multiplier = multiplier)
        var state = BackoffState(0, None, 1, config)
        
        // Test first few progressions
        for (_ <- 1 to 3) {
          val oldBackoff = state.currentBackoffSeconds
          state = state.nextBackoff
          
          // Should grow by multiplier or hit max
          val expectedBackoff = math.min(100, math.max(1, oldBackoff * multiplier))
          state.currentBackoffSeconds shouldBe expectedBackoff
        }
      }
    }

    "test edge cases and extreme values" in {
      // Test with min = max (no growth possible)
      val noGrowthConfig = createTestConfig(minBackoff = 5, maxBackoff = 5, multiplier = 2)
      val noGrowthState = BackoffState(0, None, 5, noGrowthConfig)
      val nextNoGrowth = noGrowthState.nextBackoff
      
      nextNoGrowth.currentBackoffSeconds shouldBe 5
      nextNoGrowth.consecutiveFailures shouldBe 1
      
      // Test with very large multiplier
      val largeMultiplierConfig = createTestConfig(minBackoff = 1, maxBackoff = 1000, multiplier = 100)
      val largeState = BackoffState(0, None, 10, largeMultiplierConfig)
      val nextLarge = largeState.nextBackoff
      
      nextLarge.currentBackoffSeconds should be <= 1000
      
      // Test with multiplier of 1 (no exponential growth)
      val flatConfig = createTestConfig(minBackoff = 2, maxBackoff = 20, multiplier = 1)
      val flatState = BackoffState(0, None, 5, flatConfig)
      val nextFlat = flatState.nextBackoff
      
      // Should still respect min/max bounds
      nextFlat.currentBackoffSeconds should be >= 2
      nextFlat.currentBackoffSeconds should be <= 20
    }

    "test high consecutive failure counts" in {
      val config = createTestConfig()
      var state = BackoffState(0, None, 1, config)
      
      // Simulate many failures
      for (i <- 1 to 20) {
        state = state.nextBackoff
        
        state.consecutiveFailures shouldBe i
        state.lastFailureTime shouldBe defined
        state.currentBackoffSeconds should be <= config.scheduler.backoff.maxBackoffSeconds
        state.currentBackoffSeconds should be >= config.scheduler.backoff.minBackoffSeconds
      }
      
      // After many failures, should still be at max backoff
      state.currentBackoffSeconds shouldBe config.scheduler.backoff.maxBackoffSeconds
    }

    "test shouldRetry with various time scenarios" in {
      val config = createTestConfig(minBackoff = 5, maxBackoff = 30, multiplier = 2)
      val currentTime = System.currentTimeMillis()
      
      val timeScenarios = List(
        // (secondsAgo, backoffSeconds, expectedCanRetry, description)
        (0, 5, false, "Just failed, 5s backoff"),
        (3, 5, false, "3s ago, 5s backoff"),
        (5, 5, true, "Exactly 5s ago, 5s backoff"),
        (10, 5, true, "10s ago, 5s backoff"), 
        (1, 30, false, "1s ago, 30s backoff"),
        (30, 30, true, "Exactly 30s ago, 30s backoff"),
        (60, 30, true, "60s ago, 30s backoff")
      )
      
      timeScenarios.foreach { case (secondsAgo, backoffSeconds, expectedCanRetry, description) =>
        val failureTime = currentTime - (secondsAgo * 1000)
        val state = BackoffState(1, Some(failureTime), backoffSeconds, config)
        
        withClue(s"Scenario: $description") {
          state.shouldRetry shouldBe expectedCanRetry
        }
      }
    }

    "test concurrent access and state consistency" in {
      // Test that state transitions are consistent
      val config = createTestConfig()
      val originalState = BackoffState(5, Some(System.currentTimeMillis() - 10000), 8, config)
      
      // Multiple calls to nextBackoff should be consistent
      val next1 = originalState.nextBackoff
      val next2 = originalState.nextBackoff
      
      // Both should have same progression from original state
      next1.consecutiveFailures shouldBe next2.consecutiveFailures
      next1.currentBackoffSeconds shouldBe next2.currentBackoffSeconds
      
      // But lastFailureTime might differ slightly due to System.currentTimeMillis()
      next1.lastFailureTime shouldBe defined
      next2.lastFailureTime shouldBe defined
    }

    "test mathematical progression correctness" in {
      val config = createTestConfig(minBackoff = 2, maxBackoff = 128, multiplier = 2)
      var state = BackoffState(0, None, 2, config)
      
      // Test exact mathematical progression: 2, 4, 8, 16, 32, 64, 128, 128...
      val expectedValues = List(2, 4, 8, 16, 32, 64, 128)
      
      expectedValues.foreach { expectedValue =>
        state.currentBackoffSeconds shouldBe math.min(expectedValue, 128)
        state = state.nextBackoff
      }
      
      // After reaching max, should stay at max
      state.currentBackoffSeconds shouldBe 128
    }

    "test config parameter variations and conditional paths" in {
      val configVariations = List(
        // (min, max, multiplier, startValue, description)
        (1, 64, 2, 1, "Standard exponential backoff"),
        (5, 20, 3, 5, "Higher minimum with 3x multiplier"),
        (10, 10, 2, 10, "Min equals max - no growth"),
        (1, 1000, 10, 1, "Large max with aggressive multiplier"),
        (30, 60, 1, 30, "Multiplier of 1 - linear"),
        (2, 100, 2, 50, "Start above min value")
      )
      
      configVariations.foreach { case (min, max, multiplier, startValue, description) =>
        withClue(s"Config variation: $description") {
          val config = createTestConfig(min, max, multiplier)
          val state = BackoffState(0, None, startValue, config)
          val nextState = state.nextBackoff
          
          // Test conditional boundaries
          nextState.currentBackoffSeconds should be >= min
          nextState.currentBackoffSeconds should be <= max
          nextState.consecutiveFailures shouldBe 1
          nextState.lastFailureTime shouldBe defined
          
          // Test the mathematical logic
          val expectedNext = math.min(max, math.max(min, startValue * multiplier))
          nextState.currentBackoffSeconds shouldBe expectedNext
        }
      }
    }
  }
}