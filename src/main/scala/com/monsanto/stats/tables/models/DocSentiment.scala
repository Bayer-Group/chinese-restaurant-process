package com.monsanto.stats.tables.models

/**
 * Represents the embedded sentiment scores inside of various Alchemy API responses.
 */
case class DocSentiment(sentimentType: String, score: Option[String]) {
  val doubleVal = score.map(_.toDouble)
  require(sentimentType == "negative" || sentimentType == "positive" || sentimentType == "neutral")
}
