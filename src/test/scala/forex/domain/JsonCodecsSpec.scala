package forex.domain

import io.circe.parser._
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.OffsetDateTime

class JsonCodecsSpec extends AnyWordSpec with Matchers {
  import JsonCodecs._
  import Rate.Pair

  "JsonCodecs" should {
    "encode and decode Currency" in {
      val currency: Currency = Currency.USD
      val json = currency.asJson
      val decoded = decode[Currency](json.noSpaces)
      
      decoded shouldBe Right(currency)
    }

    "handle invalid currency strings" in {
      val invalidJson = "\"INVALID_CURRENCY\""
      val decoded = decode[Currency](invalidJson)
      
      decoded.isLeft shouldBe true
    }

    "encode and decode Price" in {
      val price = Price(BigDecimal("1.2345"))
      val json = price.asJson
      val decoded = decode[Price](json.noSpaces)
      
      decoded shouldBe Right(price)
    }

    "encode and decode Timestamp" in {
      val dateTime = OffsetDateTime.parse("2023-01-01T12:00:00Z")
      val timestamp = Timestamp(dateTime)
      val json = timestamp.asJson
      val decoded = decode[Timestamp](json.noSpaces)
      
      decoded shouldBe Right(timestamp)
    }

    "encode and decode Pair" in {
      val pair = Pair(Currency.USD, Currency.EUR)
      val json = pair.asJson
      val decoded = decode[Pair](json.noSpaces)
      
      decoded shouldBe Right(pair)
    }

    "encode and decode Rate" in {
      val rate = Rate(
        Pair(Currency.USD, Currency.EUR),
        Price(BigDecimal("1.20")),
        BigDecimal("1.19"),
        BigDecimal("1.21"),
        Timestamp(OffsetDateTime.parse("2023-01-01T12:00:00Z"))
      )
      
      val json = rate.asJson
      val decoded = decode[Rate](json.noSpaces)
      
      decoded shouldBe Right(rate)
    }

    "handle complex Rate JSON structure" in {
      val jsonString = """{
        "pair": {"from": "USD", "to": "EUR"},
        "price": 1.20,
        "bid": 1.19,
        "ask": 1.21,
        "timestamp": "2023-01-01T12:00:00Z"
      }"""
      
      val decoded = decode[Rate](jsonString)
      decoded.isRight shouldBe true
      
      decoded.foreach { rate =>
        rate.pair.from shouldBe Currency.USD
        rate.pair.to shouldBe Currency.EUR
        rate.price.value shouldBe BigDecimal("1.20")
        rate.bid shouldBe BigDecimal("1.19")
        rate.ask shouldBe BigDecimal("1.21")
      }
    }
  }
}