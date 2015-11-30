package com.monsanto.stats.tables.clustering

// Map from Long index to probability (0.0 to 1.0) at that index. Small counts are left out of the Map.
// Any index missing from that Map is assumed to have small value sm.
// Actually we use this before things are 0.0 to 1.0. It can also be the counts after beta is added
// (used in CRP.smoothedCounts. So just require the double is positive.
case class WeightedVector(vecMap: Map[Int, Double]) {
  val invalidValues = vecMap.values.filter(v => v < 0.0)
  require(invalidValues.isEmpty, s"invalid probability Doubles passed to WeightedVector (as values in the vecMap): $invalidValues")
  val invalidIndexes = vecMap.keys.filter(v => v < 0)
  require(invalidIndexes.isEmpty, s"invalid index Ints passed to WeightedVector (as keys in the vecMap): $invalidIndexes")
}



