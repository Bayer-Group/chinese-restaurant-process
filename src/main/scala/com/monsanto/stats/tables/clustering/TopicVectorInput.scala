package com.monsanto.stats.tables.clustering

// Map from Int index to count at that index. Zero counts are left out of the Map.
case class TopicVectorInput(id: Long, vecMap: Map[Int, Int]) {
  require(id >= 0, "negative id passed to TopicVector: $id")
  val invalidCounts = vecMap.values.filter(v => v < 0)
  require(invalidCounts.isEmpty, s"invalid count Int passed to TopicVector (as values in the vecMap): $invalidCounts")
  val invalidIndexes = vecMap.keys.filter(v => v < 0)
  require(invalidIndexes.isEmpty, s"invalid count Int passed to TopicVector (as keys in the vecMap): $invalidIndexes")
}

// Map from Int index to count at that index. Zero counts are left out of the Map.
case class TopicVectorResult(id: Long, vecMap: Map[Int, Int], belongProbability: Double) {
  require(id >= 0, "negative id passed to TopicVector: $id")
  val invalidCounts = vecMap.values.filter(v => v < 0)
  require(invalidCounts.isEmpty, s"invalid count Int passed to TopicVector (as values in the vecMap): $invalidCounts")
  val invalidIndexes = vecMap.keys.filter(v => v < 0)
  require(invalidIndexes.isEmpty, s"invalid count Int passed to TopicVector (as keys in the vecMap): $invalidIndexes")
}
