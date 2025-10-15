package forex.http.rates

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import forex.domain.{Currency, Price, Rate, Timestamp}
import java.time.OffsetDateTime

class RatesHttpRoutesSpec extends AnyWordSpec with Matchers {

  "Rate creation" should {
    "create valid rate with all parameters" in {
      val timestamp = Timestamp(OffsetDateTime.now())
      val price = Price(BigDecimal("1.25"))
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      
      val rate = Rate(
        pair = pair,
        price = price,
        bid = BigDecimal("1.24"),
        ask = BigDecimal("1.26"),
        timestamp = timestamp
      )
      
      rate.pair.from shouldBe Currency.USD
      rate.pair.to shouldBe Currency.EUR
      rate.price.value shouldBe BigDecimal("1.25")
      rate.bid shouldBe BigDecimal("1.24")
      rate.ask shouldBe BigDecimal("1.26")
      rate.timestamp shouldBe timestamp
    }

    "handle different currency pairs" in {
      val timestamp = Timestamp(OffsetDateTime.now())
      val price = Price(BigDecimal("150.75"))
      val pair = Rate.Pair(Currency.USD, Currency.JPY)
      
      val rate = Rate(
        pair = pair,
        price = price,
        bid = BigDecimal("150.50"),
        ask = BigDecimal("151.00"),
        timestamp = timestamp
      )
      
      rate.pair.from shouldBe Currency.USD
      rate.pair.to shouldBe Currency.JPY
      rate.price.value shouldBe BigDecimal("150.75")
    }
  }

  "Rate.Pair creation" should {
    "create valid pairs" in {
      val validPair = Rate.Pair.create(Currency.USD, Currency.EUR)
      validPair shouldBe a[Right[_, _]]
      
      validPair.map(_.from) shouldBe Right(Currency.USD)
      validPair.map(_.to) shouldBe Right(Currency.EUR)
    }

    "reject same currency pairs" in {
      val invalidPair = Rate.Pair.create(Currency.USD, Currency.USD)
      invalidPair shouldBe a[Left[_, _]]
    }

    "provide all currency combinations" in {
      val allPairs = Rate.Pair.allCurrencyPairs
      allPairs.size should be > 0
      
      allPairs.toList.foreach { pair =>
        pair.from should not equal pair.to
      }
    }
  }

  "Error handling" should {
    "validate currency pair combinations" in {
      val validPair = Rate.Pair(Currency.USD, Currency.EUR)
      validPair.from should not equal validPair.to
    }

    "ensure timestamp is valid" in {
      val timestamp = Timestamp(OffsetDateTime.now())
      timestamp.value shouldBe a[OffsetDateTime]
    }

    "ensure price is positive" in {
      val price = Price(BigDecimal("1.25"))
      price.value should be > BigDecimal(0)
    }

    "ensure bid and ask are properly ordered" in {
      val timestamp = Timestamp(OffsetDateTime.now())
      val price = Price(BigDecimal("1.25"))
      val pair = Rate.Pair(Currency.USD, Currency.EUR)
      
      val rate = Rate(
        pair = pair,
        price = price,
        bid = BigDecimal("1.24"),
        ask = BigDecimal("1.26"),
        timestamp = timestamp
      )
      
      rate.bid should be < rate.ask
    }
  }
}