package forex.domain

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.OffsetDateTime

class PriceSpec extends AnyWordSpec with Matchers {

  "Price" should {
    "create price with valid decimal" in {
      val price = Price(BigDecimal("1.2345"))
      price.value shouldBe BigDecimal("1.2345")
    }

    "handle zero price" in {
      val price = Price(BigDecimal.valueOf(0))
      price.value shouldBe BigDecimal.valueOf(0)
    }

    "handle large prices" in {
      val price = Price(BigDecimal("999999.99"))
      price.value shouldBe BigDecimal("999999.99")
    }

    "handle small fractional prices" in {
      val price = Price(BigDecimal("0.000001"))
      price.value shouldBe BigDecimal("0.000001")
    }
  }
}

class TimestampSpec extends AnyWordSpec with Matchers {

  "Timestamp" should {
    "create timestamp with OffsetDateTime" in {
      val now = OffsetDateTime.now()
      val timestamp = Timestamp(now)
      timestamp.value shouldBe now
    }

    "handle past dates" in {
      val pastDate = OffsetDateTime.parse("2020-01-01T00:00:00Z")
      val timestamp = Timestamp(pastDate)
      timestamp.value shouldBe pastDate
    }

    "handle future dates" in {
      val futureDate = OffsetDateTime.parse("2030-12-31T23:59:59Z")
      val timestamp = Timestamp(futureDate)
      timestamp.value shouldBe futureDate
    }
  }
}