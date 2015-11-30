package com.monsanto.stats.tables.models

/**
 * Sparse vector of counts per user.
 */
case class ClusteredUserVector(userId: Long, counts: Map[Long,Long], belongProbability: Double)
