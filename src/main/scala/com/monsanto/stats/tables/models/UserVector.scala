package com.monsanto.stats.tables.models

/**
 * Sparse vector of counts per user.
 */
case class UserVector(userId: Long, counts: Map[Long,Long])
