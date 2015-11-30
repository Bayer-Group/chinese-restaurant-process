package com.monsanto.stats.tables

import com.monsanto.stats.tables.clustering.RealRandomNumGen
import com.monsanto.stats.tables.clustering.{CRP, ModelParams, RealRandomNumGen, TopicVectorInput}

object GoForChinese extends App {

  val alpha = 5
  val beta = 5
  val iterations = 100
  val allTopicVectors = Vector.empty[TopicVectorInput]
  val topicIndexCount = allTopicVectors.length

  val params = ModelParams(topicIndexCount.toInt, alpha, beta)
  val crp = new CRP(params, allTopicVectors)
  val result = crp.findClusters(iterations, RealRandomNumGen, crp.selectCluster)
}
