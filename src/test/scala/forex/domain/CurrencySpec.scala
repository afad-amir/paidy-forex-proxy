package forex.domain

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CurrencySpec extends AnyWordSpec with Matchers {

  "Currency" should {
    "have all expected currencies" in {
      Currency.values should contain allOf (
        Currency.USD,
        Currency.EUR,
        Currency.GBP,
        Currency.JPY,
        Currency.CHF,
        Currency.CAD,
        Currency.AUD,
        Currency.SGD,
        Currency.NZD
      )
    }

    "generate correct number of combinations" in {
      val expectedCombinations = Currency.values.length * (Currency.values.length - 1)
      Currency.allCombinationsLength shouldBe expectedCombinations
    }

    "not include same currency pairs in combinations" in {
      val combinations = Currency.allCombinations.toList
      combinations.foreach { case (from, to) =>
        from should not equal to
      }
    }

    "generate bidirectional pairs" in {
      val combinations = Currency.allCombinations.toList
      
      val usdEur = combinations.find { case (from, to) => from == Currency.USD && to == Currency.EUR }
      val eurUsd = combinations.find { case (from, to) => from == Currency.EUR && to == Currency.USD }
      
      usdEur shouldBe defined
      eurUsd shouldBe defined
    }

    "have correct entry names" in {
      Currency.USD.entryName shouldBe "USD"
      Currency.EUR.entryName shouldBe "EUR"
      Currency.GBP.entryName shouldBe "GBP"
    }

    "parse from string correctly" in {
      Currency.withNameOption("USD") shouldBe Some(Currency.USD)
      Currency.withNameOption("INVALID") shouldBe None
    }
  }
}