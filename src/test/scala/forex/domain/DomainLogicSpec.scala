package forex.domain

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.services.rates.errors._
import java.time.OffsetDateTime

class DomainLogicSpec extends AnyWordSpec with Matchers {

  "Currency validation logic" should {
    "validate fromString method with different inputs" in {
      val validInputs = List("USD", "EUR", "GBP", "JPY")
      val invalidInputs = List("XYZ", "123", "", "INVALID", "US", "USDD")
      
      validInputs.foreach { input =>
        val result = Currency.fromString(input)
        result shouldBe a[Right[_, _]]
        
        result match {
          case Right(currency) => currency shouldBe a[Currency]
          case Left(_) => fail(s"Expected valid currency for $input")
        }
      }
      
      invalidInputs.foreach { input =>
        val result = Currency.fromString(input)
        result shouldBe a[Left[_, _]]
        
        result match {
          case Left(error) => error shouldBe a[Error]
          case Right(_) => fail(s"Expected error for invalid input $input")
        }
      }
      
      // Test case sensitivity - Currency.fromString is case insensitive
      val lowercaseResult = Currency.fromString("usd")
      lowercaseResult shouldBe Right(Currency.USD)
    }

    "handle all currency combinations correctly" in {
      val currencies = List(Currency.USD, Currency.EUR, Currency.GBP, Currency.JPY)
      var validPairCount = 0
      var invalidPairCount = 0
      
      currencies.foreach { from =>
        currencies.foreach { to =>
          val pairResult = Rate.Pair.create(from, to)
          
          if (from == to) {
            // Same currency pair should be invalid
            pairResult shouldBe a[Left[_, _]]
            pairResult match {
              case Left(Error.DoublePair) => invalidPairCount += 1
              case Left(other) => fail(s"Expected DoublePair error, got $other")
              case Right(_) => fail("Expected error for same currency pair")
            }
          } else {
            // Different currency pair should be valid
            pairResult shouldBe a[Right[_, _]]
            pairResult match {
              case Right(pair) => 
                pair.from shouldBe from
                pair.to shouldBe to
                validPairCount += 1
              case Left(error) => fail(s"Expected valid pair, got error $error")
            }
          }
        }
      }
      
      // We have 4 currencies, so 4*4 = 16 total combinations
      // 4 of these are invalid (same currency), 12 are valid
      validPairCount shouldBe 12
      invalidPairCount shouldBe 4
    }
  }

  "Rate validation logic" should {
    "validate rate creation with different price scenarios" in {
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      val timestamp = Timestamp(OffsetDateTime.now())
      
      val priceScenarios = List(
        (BigDecimal("1.25"), BigDecimal("1.24"), BigDecimal("1.26"), true),   // Normal spread
        (BigDecimal("1.25"), BigDecimal("1.26"), BigDecimal("1.24"), false),  // Inverted bid/ask
        (BigDecimal("0"), BigDecimal("0"), BigDecimal("0"), true),             // Zero prices
        (BigDecimal("100.50"), BigDecimal("100.00"), BigDecimal("101.00"), true), // Large numbers
        (BigDecimal("0.0001"), BigDecimal("0.00009"), BigDecimal("0.00011"), true) // Small numbers
      )
      
      priceScenarios.foreach { case (price, bid, ask, isValidSpread) =>
        val rate = Rate(
          pair = pair,
          price = Price(price),
          bid = bid,
          ask = ask,
          timestamp = timestamp
        )
        
        rate.pair shouldBe pair
        rate.price.value shouldBe price
        rate.bid shouldBe bid
        rate.ask shouldBe ask
        
        if (isValidSpread && bid < ask) {
          rate.bid should be < rate.ask
          rate.price.value should be >= rate.bid
          rate.price.value should be <= rate.ask
        } else if (!isValidSpread) {
          // In real trading, bid should be less than ask
          // But our domain model allows any values for testing
          rate.bid shouldBe bid
          rate.ask shouldBe ask
        }
      }
    }

    "handle timestamp validation" in {
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      val price = Price(BigDecimal("1.25"))
      val now = OffsetDateTime.now()
      
      val timestampScenarios = List(
        now,                          // Current time
        now.minusHours(1),           // 1 hour ago
        now.minusDays(1),            // 1 day ago
        now.plusHours(1),            // 1 hour in future (for testing)
        now.minusYears(1)            // 1 year ago
      )
      
      timestampScenarios.foreach { testTime =>
        val timestamp = Timestamp(testTime)
        val rate = Rate(
          pair = pair,
          price = price,
          bid = BigDecimal("1.24"),
          ask = BigDecimal("1.26"),
          timestamp = timestamp
        )
        
        rate.timestamp.value shouldBe testTime
        rate.timestamp.value shouldBe a[OffsetDateTime]
        
        // Test age calculation
        val ageSeconds = java.time.Duration.between(testTime, now).getSeconds
        if (testTime.isBefore(now)) {
          ageSeconds should be >= 0L
        } else {
          // Future timestamp
          ageSeconds should be <= 0L
        }
      }
    }
  }

  "Error handling logic" should {
    "handle different error types correctly" in {
      val errorScenarios = List(
        Error.CurrencyNotSupported(None),
        Error.CurrencyNotSupported(Some("XYZ")),
        Error.DoublePair,
        Error.RateLookupFailed("Service unavailable"),
        Error.RateLookupFailed("Network timeout")
      )
      
      errorScenarios.foreach { error =>
        error match {
          case Error.CurrencyNotSupported(requestedCurr) =>
            if (requestedCurr.isDefined) {
              requestedCurr.get should not be empty
            } else {
              requestedCurr shouldBe None
            }
            
          case Error.DoublePair =>
            error shouldBe Error.DoublePair
            
          case Error.RateLookupFailed(msg) =>
            msg should not be empty
        }
      }
    }

    "validate error conversion scenarios" in {
      import forex.programs.rates.errors.toProgramError
      
      val serviceErrors = List(
        forex.services.rates.errors.Error.CurrencyNotSupported(None),
        forex.services.rates.errors.Error.CurrencyNotSupported(Some("XYZ")),
        forex.services.rates.errors.Error.DoublePair,
        forex.services.rates.errors.Error.RateLookupFailed("Test error")
      )
      
      serviceErrors.foreach { serviceError =>
        val programError = toProgramError(serviceError)
        
        (serviceError, programError) match {
          case (forex.services.rates.errors.Error.CurrencyNotSupported(curr), 
                forex.programs.rates.errors.Error.CurrencyNotSupported(convertedCurr)) =>
            convertedCurr shouldBe curr
            
          case (forex.services.rates.errors.Error.DoublePair, 
                forex.programs.rates.errors.Error.DoublePair) =>
            programError shouldBe forex.programs.rates.errors.Error.DoublePair
            
          case (forex.services.rates.errors.Error.RateLookupFailed(msg), 
                forex.programs.rates.errors.Error.RateLookupFailed(convertedMsg)) =>
            convertedMsg shouldBe msg
            
          case _ =>
            fail(s"Unexpected error conversion: $serviceError -> $programError")
        }
      }
    }
  }

  "Price and Timestamp validation" should {
    "handle Price edge cases" in {
      val priceValues = List(
        BigDecimal("0"),
        BigDecimal("0.0001"),
        BigDecimal("1.0"),
        BigDecimal("1000.25"),
        BigDecimal("999999.999999")
      )
      
      priceValues.foreach { value =>
        val price = Price(value)
        price.value shouldBe value
        
        if (value > 0) {
          price.value should be > BigDecimal("0")
        } else {
          price.value shouldBe BigDecimal("0")
        }
      }
    }

    "handle Timestamp edge cases" in {
      val now = OffsetDateTime.now()
      val timestampValues = List(
        now,
        now.withNano(0),                    // No nanoseconds
        now.withSecond(0).withNano(0),     // No seconds or nanos
        now.minusYears(10),                // Old timestamp
        now.plusYears(1)                   // Future timestamp
      )
      
      timestampValues.foreach { value =>
        val timestamp = Timestamp(value)
        timestamp.value shouldBe value
        timestamp.value shouldBe a[OffsetDateTime]
        
        // Verify the timestamp is properly constructed
        val reconstructed = OffsetDateTime.parse(value.toString)
        reconstructed shouldBe value
      }
    }
  }

  "Currency pair generation logic" should {
    "generate correct number of combinations" in {
      val allPairs = Rate.Pair.allCurrencyPairs
      val allCurrencies = Currency.values
      
      // Should generate all combinations except same-currency pairs
      // 9 currencies × 8 other currencies = 72 combinations
      val expectedCount = allCurrencies.length * (allCurrencies.length - 1)
      allPairs.size shouldBe expectedCount
      
      // Verify no same-currency pairs exist
      allPairs.toList.foreach { pair =>
        pair.from should not equal pair.to
      }
      
      // Verify all currencies are represented as 'from'
      val fromCurrencies = allPairs.map(_.from).toList.distinct
      fromCurrencies should contain theSameElementsAs allCurrencies
      
      // Verify all currencies are represented as 'to'
      val toCurrencies = allPairs.map(_.to).toList.distinct
      toCurrencies should contain theSameElementsAs allCurrencies
    }

    "handle bidirectional pairs correctly" in {
      val currencies = List(Currency.USD, Currency.EUR)
      var pairCount = 0
      
      currencies.foreach { from =>
        currencies.foreach { to =>
          if (from != to) {
            val pair = Rate.Pair(from, to)
            pair.from shouldBe from
            pair.to shouldBe to
            pairCount = pairCount + 1
            
            // Test that reverse pair is different
            val reversePair = Rate.Pair(to, from)
            reversePair.from shouldBe to
            reversePair.to shouldBe from
            
            pair should not equal reversePair
          }
        }
      }
      
      // With 2 currencies, we should have 2 valid pairs (USD->EUR, EUR->USD)
      pairCount shouldBe 2
    }
  }
}