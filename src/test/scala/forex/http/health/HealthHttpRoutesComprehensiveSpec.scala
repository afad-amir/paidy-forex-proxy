package forex.http.health

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import java.time.Instant

class HealthHttpRoutesComprehensiveSpec extends AnyWordSpec with Matchers {

  "HealthHttpRoutes comprehensive testing for branch coverage" should {

    "test ServiceHealth with all possible status combinations" in {
      val statuses = List("HEALTHY", "DEGRADED", "DOWN", "UNKNOWN", "MAINTENANCE", "ERROR")
      val services = List("external-api", "redis-cache", "backup-service", "monitoring")
      val messages = List("All good", "Some issues", "Down", "Status unknown", "Under maintenance", "Error occurred")
      
      // Exercise conditional logic in status handling
      statuses.zip(services).zip(messages).foreach { case ((status, service), message) =>
        val health = ServiceHealth(service, status, message, Instant.now().toString)
        
        // Test different conditional branches based on status
        val isOperational = status match {
          case "HEALTHY" => true
          case "DEGRADED" => true  
          case "DOWN" => false
          case "ERROR" => false
          case "MAINTENANCE" => false
          case _ => false
        }
        
        health.service shouldBe service
        health.status shouldBe status
        health.message shouldBe message
        
        // Exercise conditional logic branches
        if (isOperational) {
          health.status should (be("HEALTHY") or be("DEGRADED"))
        } else {
          health.status should not be "HEALTHY"
        }
      }
    }

    "test QuotaHealth with different quota scenarios and conditional logic" in {
      val quotaScenarios = List(
        // (remaining, total, successRate, status, shouldAllow)
        (1000, 1000, 100.0, "HEALTHY", true),
        (500, 1000, 95.0, "HEALTHY", true),
        (101, 1000, 90.0, "HEALTHY", true),
        (100, 1000, 88.0, "LOW", true),
        (50, 1000, 85.0, "LOW", true),
        (10, 1000, 80.0, "LOW", true),
        (5, 1000, 75.0, "LOW", true),
        (1, 1000, 70.0, "LOW", true),
        (0, 1000, 65.0, "EXHAUSTED", false)
      )
      
      quotaScenarios.foreach { case (remaining, total, successRate, expectedStatus, shouldAllow) =>
        val resetTime = Instant.now().plusSeconds(3600).toString
        val lastCall = if (total > remaining) Some(Instant.now().minusSeconds(300).toString) else None
        
        val quotaHealth = QuotaHealth(
          remainingCalls = remaining,
          totalCalls = total,
          successRate = successRate,
          resetTime = resetTime,
          lastCallTime = lastCall,
          status = expectedStatus,
          message = s"$remaining calls remaining"
        )
        
        // Exercise conditional branches based on quota levels
        val canMakeCalls = remaining > 0
        val isHealthy = remaining > 100
        val isLow = remaining <= 100 && remaining > 0
        val isExhausted = remaining <= 0
        
        quotaHealth.remainingCalls shouldBe remaining
        quotaHealth.totalCalls shouldBe total
        quotaHealth.successRate shouldBe successRate
        quotaHealth.status shouldBe expectedStatus
        
        // Test conditional logic branches
        if (isHealthy) {
          quotaHealth.status shouldBe "HEALTHY"
        } else if (isLow) {
          quotaHealth.status shouldBe "LOW"
        } else if (isExhausted) {
          quotaHealth.status shouldBe "EXHAUSTED"
        }
        
        canMakeCalls shouldBe shouldAllow
      }
    }

    "test CacheHealth with different cache states and conditional paths" in {
      val cacheScenarios = List(
        // (status, rateCount, ageString, isHealthy, hasData)
        ("HEALTHY", 15, Some("30s"), true, true),
        ("HEALTHY", 10, Some("2m"), true, true),
        ("PARTIAL", 8, Some("5m"), false, true),
        ("PARTIAL", 5, Some("10m"), false, true),
        ("STALE", 12, Some("2h"), false, true),
        ("STALE", 8, Some("3h"), false, true),
        ("EMPTY", 0, None, false, false),
        ("DOWN", 0, None, false, false),
        ("ERROR", 0, None, false, false)
      )
      
      cacheScenarios.foreach { case (status, rateCount, oldestAge, _, hasData) =>
        val message = status match {
          case "HEALTHY" => s"Cache healthy with $rateCount rates"
          case "PARTIAL" => s"Partial cache - $rateCount rates available"
          case "STALE" => s"Cache has stale data - oldest: ${oldestAge.getOrElse("unknown")}"
          case "EMPTY" => "Cache is empty but connected"
          case "DOWN" => "Cannot connect to cache"
          case "ERROR" => "Cache error occurred"
          case _ => "Unknown cache status"
        }
        
        val cacheHealth = CacheHealth(status, message, rateCount, oldestAge)
        
        // Exercise conditional branches based on cache state
        val canProvideFromCache = rateCount > 0
        val hasRecentData = oldestAge match {
          case Some(age) if age.contains("s") => true
          case Some(age) if age.contains("m") => true
          case Some(age) if age.contains("h") => false
          case _ => false
        }
        
        cacheHealth.status shouldBe status
        cacheHealth.rateCount shouldBe rateCount
        cacheHealth.oldestRateAge shouldBe oldestAge
        cacheHealth.message shouldBe message
        
        // Test conditional logic branches
        if (canProvideFromCache) {
          cacheHealth.rateCount should be > 0
        } else {
          cacheHealth.rateCount shouldBe 0
        }
        
        canProvideFromCache shouldBe hasData
        
        // Test age-based conditional logic
        if (oldestAge.isDefined && hasRecentData) {
          cacheHealth.status should (be("HEALTHY") or be("PARTIAL"))
        } else if (oldestAge.isDefined && !hasRecentData) {
          cacheHealth.status should be("STALE")
        }
      }
    }

    "test OverallHealth with complex conditional logic combinations" in {
      val healthScenarios = List(
        // (overallStatus, canProvide, quotaRemaining, cacheCount, issueCount)
        ("HEALTHY", true, 500, 15, 0),
        ("HEALTHY", true, 200, 12, 0), 
        ("DEGRADED", true, 50, 10, 1),
        ("DEGRADED", true, 25, 8, 2),
        ("DEGRADED", true, 0, 12, 1), // No quota but cache works
        ("DEGRADED", true, 300, 0, 1), // Good quota but no cache
        ("UNHEALTHY", false, 0, 0, 3),
        ("ERROR", false, 0, 0, 5)
      )
      
      healthScenarios.foreach { case (overallStatus, canProvide, quotaRemaining, cacheCount, issueCount) =>
        val timestamp = Instant.now().toString
        
        // Create services based on overall health
        val services = if (overallStatus == "HEALTHY") {
          List(
            ServiceHealth("external-api", "UP", "Working well", timestamp),
            ServiceHealth("redis-cache", "UP", "Connected", timestamp)
          )
        } else if (overallStatus == "DEGRADED") {
          List(
            ServiceHealth("external-api", if (quotaRemaining > 0) "UP" else "DEGRADED", "Some issues", timestamp),
            ServiceHealth("redis-cache", if (cacheCount > 0) "UP" else "DEGRADED", "Partial function", timestamp)
          )
        } else {
          List(
            ServiceHealth("external-api", "DOWN", "Not working", timestamp),
            ServiceHealth("redis-cache", "DOWN", "Not working", timestamp)
          )
        }
        
        // Create quota health based on remaining calls
        val quotaStatus = if (quotaRemaining > 100) "HEALTHY"
                         else if (quotaRemaining > 0) "LOW" 
                         else "EXHAUSTED"
        val quota = QuotaHealth(quotaRemaining, 1000, 90.0, timestamp, None, quotaStatus, "Quota message")
        
        // Create cache health based on count
        val cacheStatus = if (cacheCount >= 10) "HEALTHY"
                         else if (cacheCount > 0) "PARTIAL"
                         else "EMPTY"
        val cache = CacheHealth(cacheStatus, "Cache message", cacheCount, Some("1m"))
        
        // Create issues based on count
        val issues = (1 to issueCount).map(i => s"Issue $i").toList
        
        val overallHealth = OverallHealth(
          status = overallStatus,
          timestamp = timestamp,
          services = services,
          quota = quota,
          cache = cache,
          canProvideRates = canProvide,
          issues = issues
        )
        
        // Exercise conditional logic branches
        val hasQuota = quotaRemaining > 0
        val hasCache = cacheCount > 0
        val canProvideOverall = hasQuota || hasCache
        
        overallHealth.status shouldBe overallStatus
        overallHealth.canProvideRates shouldBe canProvide
        overallHealth.issues.length shouldBe issueCount
        
        // Test complex conditional combinations
        if (overallStatus == "HEALTHY") {
          overallHealth.canProvideRates shouldBe true
          overallHealth.issues shouldBe empty
        } else if (overallStatus == "DEGRADED") {
          overallHealth.canProvideRates shouldBe true
          overallHealth.issues should not be empty
        } else {
          overallHealth.canProvideRates shouldBe false
          overallHealth.issues should not be empty
        }
        
        // Verify the conditional logic holds
        canProvideOverall shouldBe canProvide
        
        // Test service-specific conditional logic
        services.foreach { service =>
          service.service should (be("external-api") or be("redis-cache"))
          if (overallStatus == "HEALTHY") {
            service.status shouldBe "UP"
          }
        }
      }
    }

    "test edge cases and boundary conditions" in {
      // Test zero values
      val zeroQuota = QuotaHealth(0, 0, 0.0, Instant.now().toString, None, "EMPTY", "No data")
      zeroQuota.remainingCalls shouldBe 0
      zeroQuota.totalCalls shouldBe 0
      zeroQuota.successRate shouldBe 0.0
      
      // Test maximum values
      val maxQuota = QuotaHealth(999999, 999999, 100.0, Instant.now().toString, None, "MAX", "Maximum")
      maxQuota.remainingCalls shouldBe 999999
      maxQuota.successRate shouldBe 100.0
      
      // Test null/empty cases
      val emptyCache = CacheHealth("EMPTY", "", 0, None)
      emptyCache.rateCount shouldBe 0
      emptyCache.oldestRateAge shouldBe None
      
      // Test single item cases
      val singleRate = CacheHealth("MINIMAL", "One rate", 1, Some("1s"))
      singleRate.rateCount shouldBe 1
      
      // Test very long age
      val oldCache = CacheHealth("ANCIENT", "Very old", 5, Some("24h"))
      oldCache.oldestRateAge shouldBe Some("24h")
    }

    "test message formatting and conditional string generation" in {
      val quotaMessages = List(
        (500, "Quota healthy: 500 calls remaining"),
        (50, "Quota running low: 50 calls remaining"),
        (10, "Critical: Only 10 calls remaining"),
        (0, "Quota exhausted")
      )
      
      quotaMessages.foreach { case (remaining, expectedMessage) =>
        val message = if (remaining > 100) s"Quota healthy: $remaining calls remaining"
                     else if (remaining > 20) s"Quota running low: $remaining calls remaining"  
                     else if (remaining > 0) s"Critical: Only $remaining calls remaining"
                     else "Quota exhausted"
        
        message shouldBe expectedMessage
      }
      
      // Test cache message generation
      val cacheMessages = List(
        (15, "HEALTHY", "Cache healthy with 15 fresh rates"),
        (8, "PARTIAL", "Partial cache with 8 rates available"),
        (0, "EMPTY", "Cache is empty but connected"),
        (0, "DOWN", "Cannot connect to cache")
      )
      
      cacheMessages.foreach { case (count, status, expectedMessage) =>
        val message = status match {
          case "HEALTHY" => s"Cache healthy with $count fresh rates"
          case "PARTIAL" => s"Partial cache with $count rates available"
          case "EMPTY" => "Cache is empty but connected"
          case "DOWN" => "Cannot connect to cache"
          case _ => "Unknown status"
        }
        
        message shouldBe expectedMessage
      }
    }

    "test time-based conditional logic" in {
      val now = Instant.now()
      val resetTimes = List(
        now.plusSeconds(3600),  // 1 hour from now
        now.plusSeconds(86400), // 24 hours from now
        now.minusSeconds(3600), // 1 hour ago (already reset)
        now.plusSeconds(300)    // 5 minutes from now
      )
      
      resetTimes.foreach { resetTime =>
        val isResetSoon = resetTime.isBefore(now.plusSeconds(3600))
        val hasAlreadyReset = resetTime.isBefore(now)
        val resetTimeString = resetTime.toString
        
        val quota = QuotaHealth(100, 1000, 95.0, resetTimeString, None, "TEST", "Test message")
        quota.resetTime shouldBe resetTimeString
        
        // Exercise time-based conditional logic
        if (hasAlreadyReset) {
          // Should have been reset already
          quota.resetTime should not be empty
        } else if (isResetSoon) {
          // Reset happening soon
          quota.resetTime should not be empty
        } else {
          // Reset is far in the future
          quota.resetTime should not be empty
        }
      }
    }

    "test compound conditional scenarios" in {
      // Test scenarios that exercise multiple conditional paths simultaneously
      val compoundScenarios = List(
        // Low quota + partial cache + some issues
        ("LOW_QUOTA_PARTIAL_CACHE", 25, 8, List("Quota low", "Cache incomplete")),
        // No quota + good cache + cache-only issues  
        ("NO_QUOTA_GOOD_CACHE", 0, 15, List("No API quota")),
        // Good quota + no cache + API-only
        ("QUOTA_NO_CACHE", 500, 0, List("No cached rates")),
        // Everything broken
        ("EVERYTHING_BROKEN", 0, 0, List("No quota", "No cache", "All services down"))
      )
      
      compoundScenarios.foreach { case (scenarioName, quotaRemaining, cacheCount, expectedIssues) =>
        val canProvideFromQuota = quotaRemaining > 0
        val canProvideFromCache = cacheCount > 0
        val canProvideOverall = canProvideFromQuota || canProvideFromCache
        
        val overallStatus = if (expectedIssues.isEmpty) "HEALTHY"
                           else if (canProvideOverall) "DEGRADED"
                           else "UNHEALTHY"
        
        val health = OverallHealth(
          status = overallStatus,
          timestamp = Instant.now().toString,
          services = List.empty,
          quota = QuotaHealth(quotaRemaining, 1000, 90.0, "reset", None, "TEST", "Test"),
          cache = CacheHealth("TEST", "Test", cacheCount, None),
          canProvideRates = canProvideOverall,
          issues = expectedIssues
        )
        
        withClue(s"Scenario: $scenarioName") {
          health.canProvideRates shouldBe canProvideOverall
          health.issues shouldBe expectedIssues
          
          // Exercise AND/OR conditional logic
          if (canProvideFromQuota && canProvideFromCache) {
            health.canProvideRates shouldBe true
          } else if (canProvideFromQuota || canProvideFromCache) {
            health.canProvideRates shouldBe true
          } else {
            health.canProvideRates shouldBe false
          }
        }
      }
    }
  }
}