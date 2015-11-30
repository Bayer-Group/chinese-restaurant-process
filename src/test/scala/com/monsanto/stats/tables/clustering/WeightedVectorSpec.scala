package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables._
import com.monsanto.stats.tables.UnitSpec

class WeightedVectorSpec extends UnitSpec {
  "A WeightedVector" should {
    "require that all doubles in the passed map be positive between 0.0 and 1.0" in {
      an [IllegalArgumentException] should be thrownBy WeightedVector(Map(0 -> 0.0, 1 -> 0.1, 2 -> 1.0, 3 -> -0.3))
      noException should be thrownBy WeightedVector(Map(0 -> 0.0, 1 -> 0.1, 2 -> 1.0, 3 -> 30.3))
    }
    "require that all ints in the passed map are greater than or equal to 0" in {
      an [IllegalArgumentException] should be thrownBy WeightedVector(Map(0 -> 0.0, 1 -> 0.1, -2 -> 1.0, 3 -> 0.3))
    }
  }
}
