package forex.domain

import cats.Show
import forex.services.rates.errors.Error
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.OffsetDateTime

class RateSpec extends AnyWordSpec with Matchers {
  import Rate.Pair

  "Rate.Pair" should {
    "create valid pairs for different currencies" in {
      val result = Pair.create(Currency.USD, Currency.EUR)
      result shouldBe Right(Pair(Currency.USD, Currency.EUR))
    }

    "reject pairs with same currency" in {
      val result = Pair.create(Currency.USD, Currency.USD)
      result shouldBe Left(Error.DoublePair)
    }

    "show pair correctly" in {
      val pair = Pair(Currency.USD, Currency.EUR)
      Show[Pair].show(pair) shouldBe "USDEUR"
    }

    "generate all currency pairs correctly" in {
      val allPairs = Pair.allCurrencyPairs
      allPairs.length shouldBe Currency.allCombinationsLength
      
      val usdEurPair = allPairs.toList.find(p => p.from == Currency.USD && p.to == Currency.EUR)
      usdEurPair shouldBe defined
    }
  }

  "Rate" should {
    val validDateTime = OffsetDateTime.now()

    "create valid rate with different currencies" in {
      val result = Rate.create(
        Currency.USD,
        Currency.EUR,
        BigDecimal("1.20"),
        BigDecimal("1.19"),
        BigDecimal("1.21"),
        validDateTime
      )

      result.isRight shouldBe true
      result.foreach { rate =>
        rate.pair.from shouldBe Currency.USD
        rate.pair.to shouldBe Currency.EUR
        rate.price.value shouldBe BigDecimal("1.20")
        rate.bid shouldBe BigDecimal("1.19")
        rate.ask shouldBe BigDecimal("1.21")
      }
    }

    "reject rate creation with same currencies" in {
      val result = Rate.create(
        Currency.USD,
        Currency.USD,
        BigDecimal("1.20"),
        BigDecimal("1.19"),
        BigDecimal("1.21"),
        validDateTime
      )

      result shouldBe Left(Error.DoublePair)
    }
  }
}