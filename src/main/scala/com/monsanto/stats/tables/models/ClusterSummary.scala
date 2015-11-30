package com.monsanto.stats.tables.models

/**
 * Cluster Summary
 */
case class ClusterSummary(userVectors: Vector[ClusteredUserVector], counts: Map[Int,Int])

