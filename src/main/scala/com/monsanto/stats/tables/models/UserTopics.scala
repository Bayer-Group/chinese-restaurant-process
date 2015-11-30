package com.monsanto.stats.tables.models

/**
 * Topics and counts from user text.
 */
case class UserTopics(userId: Long, topics: Map[String,Long])

