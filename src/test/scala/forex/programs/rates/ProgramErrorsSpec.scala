package forex.programs.rates

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.domain.{Currency, Rate}
import forex.programs.rates.errors._
import forex.programs.rates.Protocol._

class ProgramErrorsSpec extends AnyWordSpec with Matchers {

  "RateProgramError" should {
    "handle RateLookupFailed error" in {
      val error = Error.RateLookupFailed("Rate not available")
      error.msg shouldBe "Rate not available"
    }

    "handle CurrencyNotSupported error" in {
      val error = Error.CurrencyNotSupported(Some("XYZ"))
      error.requestedCurr shouldBe Some("XYZ")
    }

    "handle DoublePair error" in {
      val error = Error.DoublePair
      error shouldBe Error.DoublePair
    }

    "convert service errors to program errors" in {
      val serviceError = forex.services.rates.errors.Error.RateLookupFailed("Service error")
      val programError = toProgramError(serviceError)
      
      programError shouldBe a[Error.RateLookupFailed]
    }
  }

  "GetRatesRequest" should {
    "be properly constructed" in {
      val request = GetRatesRequest(Currency.USD, Currency.EUR)
      
      request.from shouldBe Currency.USD
      request.to shouldBe Currency.EUR
    }

    "handle different currency combinations" in {
      val request = GetRatesRequest(Currency.GBP, Currency.JPY)
      
      request.from shouldBe Currency.GBP
      request.to shouldBe Currency.JPY
    }

    "validate currency pair creation" in {
      val request = GetRatesRequest(Currency.USD, Currency.EUR)
      val pairResult = Rate.Pair.create(request.from, request.to)
      
      pairResult shouldBe a[Right[_, _]]
      pairResult.map(_.from) shouldBe Right(request.from)
      pairResult.map(_.to) shouldBe Right(request.to)
    }

    "reject same currency pairs" in {
      val request = GetRatesRequest(Currency.USD, Currency.USD)
      val pairResult = Rate.Pair.create(request.from, request.to)
      
      pairResult shouldBe a[Left[_, _]]
    }
  }

  "Protocol validation" should {
    "ensure valid currency pairs" in {
      val request = GetRatesRequest(Currency.USD, Currency.EUR)
      request.from should not equal request.to
    }

    "validate currency enumeration" in {
      val validCurrencies = List(Currency.USD, Currency.EUR, Currency.GBP, Currency.JPY)
      
      validCurrencies.foreach { currency =>
        currency shouldBe a[Currency]
      }
    }

    "ensure all currency combinations are valid" in {
      val allCurrencies = List(Currency.USD, Currency.EUR, Currency.GBP, Currency.JPY)
      
      allCurrencies.foreach { from =>
        allCurrencies.foreach { to =>
          if (from != to) {
            val request = GetRatesRequest(from, to)
            request.from shouldBe from
            request.to shouldBe to
          }
        }
      }
    }
  }

  "Error conversion" should {
    "convert RateLookupFailed correctly" in {
      val serviceError = forex.services.rates.errors.Error.RateLookupFailed("Test message")
      val programError = toProgramError(serviceError)
      
      programError match {
        case Error.RateLookupFailed(msg) => msg shouldBe "Test message"
        case _ => fail("Expected RateLookupFailed")
      }
    }

    "convert CurrencyNotSupported correctly" in {
      val serviceError = forex.services.rates.errors.Error.CurrencyNotSupported(Some("XYZ"))
      val programError = toProgramError(serviceError)
      
      programError match {
        case Error.CurrencyNotSupported(curr) => curr shouldBe Some("XYZ")
        case _ => fail("Expected CurrencyNotSupported")
      }
    }

    "convert DoublePair correctly" in {
      val serviceError = forex.services.rates.errors.Error.DoublePair
      val programError = toProgramError(serviceError)
      
      programError shouldBe Error.DoublePair
    }
  }
}