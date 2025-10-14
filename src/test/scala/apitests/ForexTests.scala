package apitests

import forex.domain.Currency
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ForexTests extends AnyWordSpec with Matchers {
  "currency pair validity" should {

    "validate total combinations/permutations" in {
      
      val combinationLength = 72
      Currency.allCombinationsLength shouldBe combinationLength
    }

  }
}
