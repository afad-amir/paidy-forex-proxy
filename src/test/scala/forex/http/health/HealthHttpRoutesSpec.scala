package forex.http.health

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HealthHttpRoutesSpec extends AnyWordSpec with Matchers {

  "ServiceHealth" should {
    "be properly constructed" in {
      val serviceHealth = ServiceHealth(
        service = "external-api",
        status = "HEALTHY",
        message = "All systems operational",
        lastChecked = "2024-01-01T12:00:00Z"
      )
      
      serviceHealth.service shouldBe "external-api"
      serviceHealth.status shouldBe "HEALTHY"
      serviceHealth.message shouldBe "All systems operational"
      serviceHealth.lastChecked shouldBe "2024-01-01T12:00:00Z"
    }

    "handle unhealthy status" in {
      val serviceHealth = ServiceHealth(
        service = "cache",
        status = "UNHEALTHY",
        message = "Connection failed",
        lastChecked = "2024-01-01T12:00:00Z"
      )
      
      serviceHealth.service shouldBe "cache"
      serviceHealth.status shouldBe "UNHEALTHY"
    }
  }

  "QuotaHealth" should {
    "represent healthy quota status" in {
      val quotaHealth = QuotaHealth(
        remainingCalls = 500,
        totalCalls = 1000,
        successRate = 99.5,
        resetTime = "2024-01-01T00:00:00Z",
        lastCallTime = Some("2024-01-01T12:00:00Z"),
        status = "HEALTHY",
        message = "Service operational"
      )
      
      quotaHealth.remainingCalls shouldBe 500
      quotaHealth.totalCalls shouldBe 1000
      quotaHealth.successRate shouldBe 99.5
      quotaHealth.status shouldBe "HEALTHY"
    }

    "represent exhausted quota status" in {
      val quotaHealth = QuotaHealth(
        remainingCalls = 0,
        totalCalls = 1000,
        successRate = 95.0,
        resetTime = "2024-01-01T00:00:00Z",
        lastCallTime = Some("2024-01-01T11:59:59Z"),
        status = "EXHAUSTED",
        message = "No quota remaining"
      )
      
      quotaHealth.remainingCalls shouldBe 0
      quotaHealth.status shouldBe "EXHAUSTED"
    }
  }

  "CacheHealth" should {
    "represent healthy cache" in {
      val cacheHealth = CacheHealth(
        status = "HEALTHY",
        message = "Cache operational",
        rateCount = 50,
        oldestRateAge = Some("5 minutes")
      )
      
      cacheHealth.status shouldBe "HEALTHY"
      cacheHealth.rateCount shouldBe 50
      cacheHealth.oldestRateAge shouldBe Some("5 minutes")
    }

    "represent empty cache" in {
      val cacheHealth = CacheHealth(
        status = "EMPTY",
        message = "No cached rates",
        rateCount = 0,
        oldestRateAge = None
      )
      
      cacheHealth.status shouldBe "EMPTY"
      cacheHealth.rateCount shouldBe 0
      cacheHealth.oldestRateAge shouldBe None
    }
  }

  "OverallHealth" should {
    "combine all health components" in {
      val services = List(
        ServiceHealth("external-api", "HEALTHY", "All systems operational", "2024-01-01T12:00:00Z"),
        ServiceHealth("cache", "HEALTHY", "Cache operational", "2024-01-01T12:00:00Z")
      )
      
      val quotaHealth = QuotaHealth(
        remainingCalls = 500,
        totalCalls = 1000,
        successRate = 99.5,
        resetTime = "2024-01-01T00:00:00Z",
        lastCallTime = Some("2024-01-01T12:00:00Z"),
        status = "HEALTHY",
        message = "Service operational"
      )
      
      val cacheHealth = CacheHealth(
        status = "HEALTHY",
        message = "Cache operational",
        rateCount = 50,
        oldestRateAge = Some("5 minutes")
      )
      
      val overallHealth = OverallHealth(
        status = "HEALTHY",
        timestamp = "2024-01-01T12:00:00Z",
        services = services,
        quota = quotaHealth,
        cache = cacheHealth,
        canProvideRates = true,
        issues = List.empty
      )
      
      overallHealth.status shouldBe "HEALTHY"
      overallHealth.canProvideRates shouldBe true
      overallHealth.issues shouldBe empty
      overallHealth.services should have size 2
    }

    "represent degraded service" in {
      val services = List(
        ServiceHealth("external-api", "LOW", "Low quota remaining", "2024-01-01T12:00:00Z"),
        ServiceHealth("cache", "STALE", "Some rates are old", "2024-01-01T12:00:00Z")
      )
      
      val quotaHealth = QuotaHealth(
        remainingCalls = 50,
        totalCalls = 1000,
        successRate = 98.0,
        resetTime = "2024-01-01T00:00:00Z",
        lastCallTime = Some("2024-01-01T12:00:00Z"),
        status = "LOW",
        message = "Limited quota"
      )
      
      val cacheHealth = CacheHealth(
        status = "STALE",
        message = "Some rates are old",
        rateCount = 20,
        oldestRateAge = Some("15 minutes")
      )
      
      val overallHealth = OverallHealth(
        status = "DEGRADED",
        timestamp = "2024-01-01T12:00:00Z",
        services = services,
        quota = quotaHealth,
        cache = cacheHealth,
        canProvideRates = true,
        issues = List("Low quota", "Stale cache data")
      )
      
      overallHealth.status shouldBe "DEGRADED"
      overallHealth.canProvideRates shouldBe true
      overallHealth.issues should contain("Low quota")
    }
  }
}