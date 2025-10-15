package forex.services.rates

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.domain.Rate
import forex.domain.Currency
import forex.services.rates.errors._

class RatesErrorsSpec extends AnyWordSpec with Matchers {

  "RatesError" should {
    "handle CurrencyNotSupported error without message" in {
      val error = Error.CurrencyNotSupported()
      error.requestedCurr shouldBe None
    }

    "handle CurrencyNotSupported error with message" in {
      val error = Error.CurrencyNotSupported(Some("XYZ"))
      error.requestedCurr shouldBe Some("XYZ")
    }

    "handle DoublePair error" in {
      val error = Error.DoublePair
      error shouldBe Error.DoublePair
    }

    "handle RateLookupFailed error" in {
      val error = Error.RateLookupFailed("Rate not found")
      error.msg shouldBe "Rate not found"
    }
  }

  "Rate.Pair" should {
    "be properly constructed" in {
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      
      pair.from shouldBe Currency.USD
      pair.to shouldBe Currency.EUR
    }

    "handle different currency combinations" in {
      val pair = Rate.Pair(Currency.GBP, Currency.JPY)
      
      pair.from shouldBe Currency.GBP
      pair.to shouldBe Currency.JPY
    }

    "provide all currency pairs" in {
      val allPairs = Rate.Pair.allCurrencyPairs
      allPairs.size should be > 0
    }
  }
}