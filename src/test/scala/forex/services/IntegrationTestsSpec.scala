package forex.services

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.config._
import forex.domain._
import forex.services.rates.errors._
import scala.concurrent.duration._
import java.time.OffsetDateTime

class IntegrationTestsSpec extends AnyWordSpec with Matchers {

  // Test configuration
  def createTestConfig: ApplicationConfig = ApplicationConfig(
    http = HttpConfig("localhost", 8080, 30.seconds),
    oneFrame = OneFrameConfig(
      uri = "http://localhost:8080",
      token = "test-token",
      quota = QuotaConfig(
        maxCallsPerDay = 1000,
        maxRateAgeSeconds = 300,
        refreshCheckInterval = 10.minutes
      )
    ),
    redis = RedisConfig("localhost", 6379, 30.seconds, 1.hour, 5.seconds, 1.second),
    scheduler = SchedulerConfig(
      backoff = BackoffConfig(
        initialBackoffSeconds = 5,
        maxBackoffSeconds = 300,
        minBackoffSeconds = 1,
        multiplier = 2
      ),
      healthCheckInterval = 30.seconds
    ),
    system = SystemConfig(retryDelay = 5.seconds)
  )

  "Domain logic integration tests" should {
    "handle currency validation with different cases" in {
      val testCases = List(
        ("USD", true),
        ("eur", true), 
        ("GBP", true),
        ("INVALID", false),
        ("", false)
      )
      
      testCases.foreach { case (input, shouldSucceed) =>
        val result = Currency.fromString(input)
        if (shouldSucceed) {
          result.isRight shouldBe true
        } else {
          result.isLeft shouldBe true
        }
      }
    }

    "handle Rate.Pair creation with validation" in {
      // Valid pairs
      val validPair1 = Rate.Pair.create(Currency.USD, Currency.EUR)
      validPair1.isRight shouldBe true
      
      val validPair2 = Rate.Pair.create(Currency.GBP, Currency.JPY)
      validPair2.isRight shouldBe true
      
      // Invalid pairs (same currency)
      val invalidPair = Rate.Pair.create(Currency.USD, Currency.USD)
      invalidPair.isLeft shouldBe true
      invalidPair.swap.getOrElse(Error.DoublePair) shouldBe Error.DoublePair
    }

    "generate all currency combinations correctly" in {
      val allCombinations = Currency.allCombinations
      val allCurrencies = Currency.values
      
      // Should have 9 currencies × 8 other currencies = 72 combinations
      allCombinations.length shouldBe (allCurrencies.length * (allCurrencies.length - 1))
      
      // No same-currency pairs should exist
      allCombinations.toList.foreach { case (from, to) =>
        from should not equal to
      }
      
      // All currencies should appear as both 'from' and 'to'
      val fromCurrencies = allCombinations.map(_._1).toList.distinct
      val toCurrencies = allCombinations.map(_._2).toList.distinct
      
      fromCurrencies should contain theSameElementsAs allCurrencies
      toCurrencies should contain theSameElementsAs allCurrencies
    }

    "handle Rate creation with different scenarios" in {
      val validPair = Rate.Pair(Currency.USD, Currency.EUR)
      val timestamp = Timestamp(OffsetDateTime.now())
      val price = Price(BigDecimal("1.2345"))
      val bid = BigDecimal("1.2340")
      val ask = BigDecimal("1.2350")
      
      // Valid rate
      val validRate = Rate(validPair, price, bid, ask, timestamp)
      validRate.pair shouldBe validPair
      validRate.price.value shouldBe BigDecimal("1.2345")
      validRate.bid shouldBe bid
      validRate.ask shouldBe ask
      
      // Test with edge case prices
      val zeroPrice = Rate(validPair, Price(BigDecimal("0")), BigDecimal("0"), BigDecimal("0"), timestamp)
      zeroPrice.price.value shouldBe BigDecimal("0")
      
      val highPrice = Rate(validPair, Price(BigDecimal("999999.99")), BigDecimal("999999.98"), BigDecimal("1000000.00"), timestamp)
      highPrice.price.value shouldBe BigDecimal("999999.99")
    }
  }

  "Scheduler BackoffState logic simulation" should {
    "simulate exponential backoff progression" in {
      val config = createTestConfig
      
      // Simulate backoff state progression
      case class BackoffState(
        consecutiveFailures: Int,
        lastFailureTime: Option[Long],
        currentBackoffSeconds: Int
      ) {
        def nextBackoff: BackoffState = {
          val nextBackoffSeconds = math.min(
            config.scheduler.backoff.maxBackoffSeconds, 
            math.max(
              config.scheduler.backoff.minBackoffSeconds, 
              currentBackoffSeconds * config.scheduler.backoff.multiplier
            )
          )
          BackoffState(
            consecutiveFailures + 1,
            Some(System.currentTimeMillis()),
            nextBackoffSeconds
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
      
      val initialState = BackoffState(0, None, config.scheduler.backoff.initialBackoffSeconds)
      
      // Initial state should allow retry
      initialState.shouldRetry shouldBe true
      initialState.consecutiveFailures shouldBe 0
      
      // After first failure
      val afterFirstFailure = initialState.nextBackoff
      afterFirstFailure.consecutiveFailures shouldBe 1
      afterFirstFailure.currentBackoffSeconds shouldBe (config.scheduler.backoff.initialBackoffSeconds * config.scheduler.backoff.multiplier)
      afterFirstFailure.shouldRetry shouldBe false // Just failed, should wait
      
      // After second failure - exponential growth
      val afterSecondFailure = afterFirstFailure.nextBackoff
      afterSecondFailure.consecutiveFailures shouldBe 2
      val expectedBackoff = config.scheduler.backoff.initialBackoffSeconds * config.scheduler.backoff.multiplier * config.scheduler.backoff.multiplier
      afterSecondFailure.currentBackoffSeconds shouldBe math.min(config.scheduler.backoff.maxBackoffSeconds, expectedBackoff)
      
      // Test max backoff limit
      var state = initialState
      for (_ <- 1 to 10) {
        state = state.nextBackoff
      }
      state.currentBackoffSeconds shouldBe config.scheduler.backoff.maxBackoffSeconds
    }

    "handle retry timing logic correctly" in {
      val config = createTestConfig
      val currentTime = System.currentTimeMillis()
      
      case class BackoffState(
        consecutiveFailures: Int,
        lastFailureTime: Option[Long],
        currentBackoffSeconds: Int
      ) {
        def shouldRetry: Boolean =
          lastFailureTime match {
            case None => true
            case Some(lastFailure) =>
              val timeSinceFailure = (System.currentTimeMillis() - lastFailure) / 1000
              timeSinceFailure >= currentBackoffSeconds
          }
      }
      
      // State with no previous failure should retry
      val noFailureState = BackoffState(0, None, config.scheduler.backoff.initialBackoffSeconds)
      noFailureState.shouldRetry shouldBe true
      
      // State with recent failure should not retry
      val recentFailureState = BackoffState(1, Some(currentTime), 10)
      recentFailureState.shouldRetry shouldBe false
      
      // State with old failure should retry
      val oldFailureState = BackoffState(1, Some(currentTime - 15000), 10) // 15 seconds ago, 10 second backoff
      oldFailureState.shouldRetry shouldBe true
    }

    "respect min and max backoff bounds" in {
      val config = createTestConfig
      
      // Test min backoff enforcement
      val minBackoff = math.max(config.scheduler.backoff.minBackoffSeconds, 1)
      minBackoff should be >= config.scheduler.backoff.minBackoffSeconds
      
      // Test max backoff enforcement
      val maxBackoff = math.min(config.scheduler.backoff.maxBackoffSeconds, 1000)
      maxBackoff shouldBe config.scheduler.backoff.maxBackoffSeconds
      
      // Test multiplier logic
      val multipliedValue = config.scheduler.backoff.initialBackoffSeconds * config.scheduler.backoff.multiplier
      val cappedValue = math.min(config.scheduler.backoff.maxBackoffSeconds, multipliedValue)
      cappedValue should be <= config.scheduler.backoff.maxBackoffSeconds
    }
  }

  "Error handling scenarios" should {
    "handle different error types correctly" in {
      import Error._
      
      // Test CurrencyNotSupported error
      val currencyError = CurrencyNotSupported(Some("INVALID"))
      currencyError shouldBe a[CurrencyNotSupported]
      
      // Test DoublePair error
      val doublePairError = DoublePair
      doublePairError shouldBe DoublePair
      
      // Test RateLookupFailed error
      val lookupError = RateLookupFailed("Redis connection failed")
      lookupError shouldBe a[RateLookupFailed]
    }

    "convert between error types correctly" in {
      import Error._
      
      // Test currency not supported scenario
      val invalidCurrencyResult = Currency.fromString("INVALID")
      invalidCurrencyResult.isLeft shouldBe true
      invalidCurrencyResult.swap.getOrElse(Error.DoublePair) shouldBe a[CurrencyNotSupported]
      
      // Test double pair scenario  
      val doublePairResult = Rate.Pair.create(Currency.USD, Currency.USD)
      doublePairResult.isLeft shouldBe true
      doublePairResult.swap.getOrElse(Error.DoublePair) shouldBe Error.DoublePair
    }
  }

  "Configuration validation" should {
    "create valid configuration with all required fields" in {
      val config = createTestConfig
      
      // Validate HTTP config
      config.http.host shouldBe "localhost"
      config.http.port shouldBe 8080
      config.http.timeout should be > 0.seconds
      
      // Validate OneFrame config  
      config.oneFrame.uri should not be empty
      config.oneFrame.token should not be empty
      config.oneFrame.quota.maxCallsPerDay should be > 0
      config.oneFrame.quota.maxRateAgeSeconds should be > 0
      
      // Validate Redis config
      config.redis.host should not be empty
      config.redis.port should be > 0
      config.redis.timeout should be > 0.seconds
      config.redis.ttl should be > 0.seconds
      
      // Validate Scheduler config
      config.scheduler.backoff.initialBackoffSeconds should be > 0
      config.scheduler.backoff.maxBackoffSeconds should be >= config.scheduler.backoff.initialBackoffSeconds
      config.scheduler.backoff.multiplier should be > 1
      
      // Validate System config
      config.system.retryDelay should be > 0.seconds
    }

    "handle configuration edge cases" in {
      val config = createTestConfig
      
      // Test quota calculations
      val dailyQuota = config.oneFrame.quota.maxCallsPerDay
      val secondsPerDay = 24 * 60 * 60
      val baseInterval = secondsPerDay.toDouble / dailyQuota
      baseInterval should be > 0.0
      
      // Test backoff progression limits
      val backoffConfig = config.scheduler.backoff
      backoffConfig.maxBackoffSeconds should be >= backoffConfig.minBackoffSeconds
      backoffConfig.initialBackoffSeconds should be >= backoffConfig.minBackoffSeconds
      backoffConfig.initialBackoffSeconds should be <= backoffConfig.maxBackoffSeconds
    }

    "validate duration and timeout settings" in {
      val config = createTestConfig
      
      // Test all durations are positive
      config.http.timeout.toSeconds should be > 0L
      config.redis.timeout.toSeconds should be > 0L
      config.redis.ttl.toSeconds should be > 0L
      config.redis.lockTtl.toSeconds should be > 0L
      config.redis.lockRetryDelay.toSeconds should be > 0L
      config.scheduler.healthCheckInterval.toSeconds should be > 0L
      config.system.retryDelay.toSeconds should be > 0L
      config.oneFrame.quota.refreshCheckInterval.toSeconds should be > 0L
      
      // Test reasonable timeout values
      config.http.timeout.toSeconds should be < 300L // Less than 5 minutes
      config.redis.timeout.toSeconds should be < 60L // Less than 1 minute
      
      // Test quota settings
      config.oneFrame.quota.maxCallsPerDay should be > 0
      config.oneFrame.quota.maxCallsPerDay should be < 100000 // Reasonable upper bound
      config.oneFrame.quota.maxRateAgeSeconds should be > 0
      config.oneFrame.quota.maxRateAgeSeconds should be < 86400 // Less than 1 day
    }
  }

  "Currency and Rate business logic" should {
    "validate currency enumeration completeness" in {
      val allCurrencies = Currency.values
      allCurrencies should contain (Currency.USD)
      allCurrencies should contain (Currency.EUR)
      allCurrencies should contain (Currency.GBP)
      allCurrencies should contain (Currency.JPY)
      allCurrencies should contain (Currency.CHF)
      allCurrencies should contain (Currency.AUD)
      allCurrencies should contain (Currency.CAD)
      allCurrencies should contain (Currency.NZD)
      allCurrencies should contain (Currency.SGD)
      
      allCurrencies.length shouldBe 9
    }

    "handle Rate.Pair business rules" in {
      val allPairs = Rate.Pair.allCurrencyPairs
      
      // Verify no self-pairs exist
      allPairs.toList.foreach { pair =>
        pair.from should not equal pair.to
      }
      
      // Verify bidirectional coverage
      val currencies = List(Currency.USD, Currency.EUR, Currency.GBP)
      currencies.foreach { from =>
        currencies.foreach { to =>
          if (from != to) {
            val expectedPair = Rate.Pair(from, to)
            allPairs.toList should contain (expectedPair)
          }
        }
      }
    }

    "handle timestamp and price validation" in {
      val now = OffsetDateTime.now()
      val pastTime = now.minusDays(1)
      val futureTime = now.plusDays(1)
      
      // All timestamps should be valid
      val currentTimestamp = Timestamp(now)
      val pastTimestamp = Timestamp(pastTime)
      val futureTimestamp = Timestamp(futureTime)
      
      currentTimestamp.value shouldBe now
      pastTimestamp.value shouldBe pastTime
      futureTimestamp.value shouldBe futureTime
      
      // Price validation
      val prices = List(
        Price(BigDecimal("0.01")),
        Price(BigDecimal("1.0")),
        Price(BigDecimal("999999.99")),
        Price(BigDecimal("0"))
      )
      
      prices.foreach { price =>
        price.value should be >= BigDecimal("0")
      }
    }

    "handle different currency pair combinations systematically" in {
      // Test all major currency combinations
      val majorCurrencies = List(Currency.USD, Currency.EUR, Currency.GBP, Currency.JPY)
      
      var validPairCount = 0
      var invalidPairCount = 0
      
      majorCurrencies.foreach { from =>
        majorCurrencies.foreach { to =>
          val pairResult = Rate.Pair.create(from, to)
          
          if (from == to) {
            // Same currency pair should be invalid
            pairResult.isLeft shouldBe true
            invalidPairCount += 1
          } else {
            // Different currency pair should be valid
            pairResult.isRight shouldBe true
            validPairCount += 1
          }
        }
      }
      
      // Verify counts
      validPairCount shouldBe (majorCurrencies.length * (majorCurrencies.length - 1))
      invalidPairCount shouldBe majorCurrencies.length
      
      // Total should equal all combinations
      (validPairCount + invalidPairCount) shouldBe (majorCurrencies.length * majorCurrencies.length)
    }

    "handle edge cases in rate construction" in {
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      val timestamp = Timestamp(OffsetDateTime.now())
      
      // Test with minimum values
      val minRate = Rate(pair, Price(BigDecimal("0.0001")), BigDecimal("0.0001"), BigDecimal("0.0002"), timestamp)
      minRate.price.value should be > BigDecimal("0")
      minRate.bid should be <= minRate.ask
      
      // Test with large values
      val maxRate = Rate(pair, Price(BigDecimal("999999")), BigDecimal("999998"), BigDecimal("1000000"), timestamp)
      maxRate.price.value should be > BigDecimal("0")
      maxRate.bid should be <= maxRate.ask
      
      // Test with equal bid/ask (edge case)
      val equalRate = Rate(pair, Price(BigDecimal("1.0")), BigDecimal("1.0"), BigDecimal("1.0"), timestamp)
      equalRate.bid shouldBe equalRate.ask
    }
  }
}