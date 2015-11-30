package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables._
import com.monsanto.stats.tables.UnitSpec

class TopicVectorResultSpec extends UnitSpec {
  "A TopicVectorInput" should {
    "require that the Long id passed is positive" in {
      an [IllegalArgumentException] should be thrownBy TopicVectorInput(-1, Map(0 -> 0, 1 -> 1, 2 -> 1, 3 -> 3))
    }
    "require that all count values in the passed map be positive" in {
      an [IllegalArgumentException] should be thrownBy TopicVectorInput(1, Map(0 -> 0, 1 -> 1, 2 -> 1, 3 -> -3))
    }
    "require that all int indexes in the passed map be positive" in {
      an [IllegalArgumentException] should be thrownBy TopicVectorInput(1, Map(0 -> 0, 1 -> 1, -2 -> 1, 3 -> 3))
    }
  }
}
