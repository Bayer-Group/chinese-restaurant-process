package com.monsanto.stats.tables.clustering

import scala.annotation.elidable
import elidable._

// Map from Int index to count at that index. Zero counts are left out of the Map.
case class ClusterResult(params: ModelParams, topicVectors: Vector[TopicVectorResult], topicCountsSums: Map[Int, Int]) {
  ClusterResult.requireTopicVectorResultsSizeLessThanOrEqualToTotal(topicVectors, params.topicVectorSize)
}

object ClusterResult {

  def apply(params: ModelParams, topicVectors: Vector[TopicVectorResult]): ClusterResult = {
    val topicCountsSums: Map[Int, Int] = computeTopicCountsSums(topicVectors)
    apply(params, topicVectors, topicCountsSums)
  }

  def computeTopicCountsSums(topicVectors: Vector[TopicVectorResult]): Map[Int, Int] = {
    val temp: Map[Int,Vector[(Int, Int)]] = topicVectors.flatMap { tv => tv.vecMap.toVector }.groupBy(_._1)
    val temp2: Map[Int,Vector[Int]] = temp.mapValues(xs => xs.map(_._2))
    temp2.mapValues(_.sum)
  }

  @elidable(ASSERTION) def requireTopicVectorResultsSizeLessThanOrEqualToTotal(topicVectors: Vector[TopicVectorResult], topicVectorSize: Int): Unit = {
    if (topicVectors exists { case TopicVectorResult(_, mp, _) => mp.size > topicVectorSize }) {
      val probs: Vector[(Long, Int)] = topicVectors.collect { case TopicVectorResult(id, mp, _) if mp.size > topicVectorSize => (id, mp.size) }
      throw new IllegalArgumentException(s"${probs.size} topic vector maps had size > params.totalTopics $topicVectorSize: ${probs}")
    }
  }
}
