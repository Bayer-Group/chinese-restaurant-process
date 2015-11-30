package com.monsanto.stats.tables.models

/**
 * General response format for Alchemy calls; most calls are a subset of these element, but
 * the combined call can return almost any combination.
 */
case class AlchemyComboResponse(
  status: String,
  statusInfo: Option[String],
  totalTransactions: String,
  language: String,
  docSentiment: Option[DocSentiment],
  keywords: Option[Array[AlchemyKeyword]],
  concepts: Option[Array[AlchemyConcept]],
  entities: Option[Array[AlchemyEntity]],
  taxonomy: Option[Array[AlchemyTaxonomy]]
) {
  val totalTransactionsInt = totalTransactions.toInt

  override def toString = {
    s"($status, $statusInfo, $totalTransactions, $language, $docSentiment, ${keywords.mkString(";")}," +
    s"${concepts.mkString(";")}, ${entities.mkString(";")}, ${taxonomy.mkString(";")})"
  }
}

case class AlchemyTaxonomyResponse(
  status: String,
  statusInfo: Option[String],
  language: String,
  taxonomy: Option[Array[AlchemyTaxonomy]]
)

case class AlchemyKeyword(text: String, relevance: String, sentiment: Option[DocSentiment]) {
  val relevanceDouble = relevance.toDouble
}

case class AlchemyConcept(text: String, relevance: String) {
  val relevanceDouble = relevance.toDouble
  // additional fields (not currently used) include a number of link fields
}

// NOTE: entityType must be mapped to 'type' in json marshalling/unmarshalling
case class AlchemyEntity(entityType: String, text: String, relevance: String, count: String, sentiment: Option[DocSentiment]) {
  val relevanceDouble  = relevance.toDouble
  val countInt = count.toInt
}

case class AlchemyTaxonomy(label: String, score: String, confident: Option[String] = None) {
  confident.foreach(c => require(c == "yes" || c == "no"))
  val scoreDouble  = score.toDouble
  // note, the default for the "confident" property is true (or "yes", presumably), which is omitted in the JSON response.
  // So, oddly, only the value "no" is seen, while None indicates "yes".
  val confidentBool = confident.map(_ == "yes").getOrElse(true)
}
