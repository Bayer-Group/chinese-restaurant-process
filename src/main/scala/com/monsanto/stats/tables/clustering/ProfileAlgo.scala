package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables._
import com.monsanto.stats.tables.utils.ProfileUtils
import ProfileUtils._

object ProfileAlgo {

  def main(args: Array[String]): Unit = {
    val allTopicVectors =
      args.headOption match {
        case Some("big") => DataGen.cannedBigData
        case _ => MnMGen.cannedData
      }
    val params =
      args.headOption match {
        case Some("big") => ModelParams(10000, 0.01, 0.01)
        case _ => ModelParams(5, 0.01, 0.01)
      }
    val crp = new CRP(params, allTopicVectors)
    val (result, millis) = time {
      crp.findClusters(100, SameRandomNumGen, crp.selectCluster, false)
    }
    result.clusters.map(_.topicCountsSums) foreach println
    println(s"Algo required: $millis milliseconds")
  }
}

