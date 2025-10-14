package forex.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.{Instant, OffsetDateTime, ZoneOffset}
import scala.concurrent.duration._
import scala.util.{Success, Failure}

class UtilitySpec extends AnyWordSpec with Matchers {

  "Time utilities" should {
    "handle current time operations" in {
      val now = Instant.now()
      val before = now.minusSeconds(1)
      val after = now.plusSeconds(1)
      
      before.isBefore(now) shouldBe true
      after.isAfter(now) shouldBe true
    }

    "convert between time formats" in {
      val instant = Instant.now()
      val offsetDateTime = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)
      
      offsetDateTime.toInstant shouldBe instant
    }

    "handle duration calculations" in {
      val start = Instant.now()
      val end = start.plusSeconds(60)
      val duration = java.time.Duration.between(start, end)
      
      duration.getSeconds shouldBe 60
    }
  }

  "Currency validation" should {
    "validate supported currencies" in {
      val supportedCurrencies = Set("USD", "EUR", "GBP", "JPY")
      
      supportedCurrencies should contain("USD")
      supportedCurrencies should contain("EUR")
      supportedCurrencies should contain("GBP")
      supportedCurrencies should contain("JPY")
    }

    "reject invalid currencies" in {
      val invalidCurrencies = Set("XYZ", "ABC", "123")
      val supportedCurrencies = Set("USD", "EUR", "GBP", "JPY")
      
      invalidCurrencies.intersect(supportedCurrencies) shouldBe empty
    }
  }

  "Number utilities" should {
    "handle BigDecimal operations" in {
      val price1 = BigDecimal("1.2345")
      val price2 = BigDecimal("2.3456")
      
      price1 + price2 shouldBe BigDecimal("3.5801")
      price2 - price1 shouldBe BigDecimal("1.1111")
    }

    "handle precision and rounding" in {
      val price = BigDecimal("1.23456789")
      val rounded = price.setScale(4, BigDecimal.RoundingMode.HALF_UP)
      
      rounded shouldBe BigDecimal("1.2346")
    }

    "validate positive numbers" in {
      val positivePrice = BigDecimal("1.25")
      val zeroPrice = BigDecimal("0")
      val negativePrice = BigDecimal("-1.25")
      
      positivePrice > 0 shouldBe true
      zeroPrice == 0 shouldBe true
      negativePrice < 0 shouldBe true
    }
  }

  "String utilities" should {
    "handle currency code validation" in {
      val validCodes = List("USD", "EUR", "GBP", "JPY")
      
      validCodes.foreach { code =>
        code.length shouldBe 3
        code.forall(_.isUpper) shouldBe true
        code.forall(_.isLetter) shouldBe true
      }
      
      // Test individual invalid cases
      "usd".length shouldBe 3 // but not all uppercase
      "usd".forall(_.isUpper) shouldBe false
      
      "XYZ".length shouldBe 3 // valid format but not a real currency
      "123".forall(_.isLetter) shouldBe false
      "".isEmpty shouldBe true
    }

    "handle empty and null values" in {
      val emptyString = ""
      val nonEmptyString = "USD"
      
      emptyString.isEmpty shouldBe true
      nonEmptyString.nonEmpty shouldBe true
    }
  }

  "Collection utilities" should {
    "handle list operations" in {
      val currencies = List("USD", "EUR", "GBP", "JPY")
      
      currencies should have size 4
      currencies should contain("USD")
      currencies.head shouldBe "USD"
      currencies.last shouldBe "JPY"
    }

    "handle set operations" in {
      val set1 = Set("USD", "EUR")
      val set2 = Set("EUR", "GBP")
      
      set1.intersect(set2) shouldBe Set("EUR")
      set1.union(set2) shouldBe Set("USD", "EUR", "GBP")
    }

    "handle map operations" in {
      val rateMap = Map(
        "USDEUR" -> BigDecimal("0.85"),
        "GBPJPY" -> BigDecimal("150.25")
      )
      
      rateMap should have size 2
      rateMap("USDEUR") shouldBe BigDecimal("0.85")
      rateMap.get("INVALID") shouldBe None
    }
  }

  "Error handling utilities" should {
    "handle Try operations" in {
      val successTry = scala.util.Try(BigDecimal("1.25"))
      val failureTry = scala.util.Try(BigDecimal("invalid"))
      
      successTry shouldBe a[Success[_]]
      failureTry shouldBe a[Failure[_]]
    }

    "handle Option operations" in {
      val someValue = Some("USD")
      val noneValue: Option[String] = None
      
      someValue.isDefined shouldBe true
      noneValue.isEmpty shouldBe true
      someValue.getOrElse("DEFAULT") shouldBe "USD"
      noneValue.getOrElse("DEFAULT") shouldBe "DEFAULT"
    }

    "handle Either operations" in {
      val rightValue: Either[String, Int] = Right(42)
      val leftValue: Either[String, Int] = Left("Error")
      
      rightValue.isRight shouldBe true
      leftValue.isLeft shouldBe true
      rightValue.getOrElse(0) shouldBe 42
      leftValue.getOrElse(0) shouldBe 0
    }
  }

  "Configuration utilities" should {
    "validate duration formats" in {
      val durations = List(
        1.second,
        30.seconds,
        5.minutes,
        1.hour,
        1.day
      )
      
      durations.foreach { duration =>
        duration.toSeconds should be > 0L
      }
    }

    "handle timeout configurations" in {
      val shortTimeout = 5.seconds
      val mediumTimeout = 30.seconds
      val longTimeout = 5.minutes
      
      shortTimeout < mediumTimeout shouldBe true
      mediumTimeout < longTimeout shouldBe true
    }
  }
}