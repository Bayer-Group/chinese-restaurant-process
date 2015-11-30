package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables.UnitSpec
import org.scalatest._
import Inspectors._
import enablers.Collecting
import scala.collection.GenTraversable
import com.monsanto.stats.tables._

class RestaurantSpec extends UnitSpec {

  val allTopicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
  val p5 = ModelParams(5, 1, 1)
  val crp = new CRP(p5, allTopicVectorResults)
  val allTopicVectors = allTopicVectorResults.map(crp.TopicVector.from(_))

  "The Restaurant companion" should {
    "offer a factory method that computes the assignments" in {
      val r = crp.Restaurant(crp.initializeClusters())
      for (topicVectorIdx <- 0 until allTopicVectors.size) {
        val clusterIdx = r.assignment(topicVectorIdx)
        r.clusters(clusterIdx).topicVectors(0) shouldEqual allTopicVectors(topicVectorIdx)
      }
/*
      forAll (r.assignments) { case (topicVectorIdx, clusterIdx) => 
        r.clusters(clusterIdx).topicVectors(0) shouldEqual r.topicVectors(topicVectorIdx)
      }
*/
    }
    "offer a logPosteriorProbability method that computes same value as an alternative version" in {

      val allTopicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
      val p5 = ModelParams(5, 1, 1)
      val crp = new CRP(p5, allTopicVectorResults)
      val allTopicVectors: Vector[crp.TopicVector] = allTopicVectorResults.map(crp.TopicVector.from(_))
      val clusters: Vector[crp.Cluster] = crp.initializeClusters(true)

      def dataLikelihoodIgnoringClusterCount: Double = {
        clusters.map(_.cValue).sum
      }

      def clusterSizeHistogram: scala.collection.Map[Int, Int] = {
        clusters.groupBy(_.size).mapValues(_.size).toMap
      }

      // CountOfClusters * ( lnGamma(beta) * vocabSize - lnGamma(beta * vocabSize) )
      def clusterCountPart(vocabSize: Int, beta: Double): Double =
        clusters.size * (crp.logGamma(beta) * vocabSize - crp.logGamma(beta * vocabSize))

      def logPosteriorProbability(params: ModelParams): Double = {
        val dLICC = dataLikelihoodIgnoringClusterCount
        val ccP = clusterCountPart(params.topicVectorSize, params.beta)
        val cA = logProbabilityOfClusteringArrangement(clusters.size, params.alpha, clusterSizeHistogram)

        (dLICC - ccP) + cA
      }

      def logProbabilityOfClusteringArrangement(
                                               clusterCount: Int,
                                               alpha: Double,
                                               clusterSizeHistogram: scala.collection.Map[Int, Int] // m
                                               ): Double = {
        // clusterCount * Math.log(alpha) - Sum_i( m_i * ln(i) + logGamma(m_i + 1) )
        val sum = clusterSizeHistogram.foldLeft(0.0) { case (sum, (clusterSize, countOfThisSize)) =>
          sum + countOfThisSize * Math.log(clusterSize) + crp.logGamma(countOfThisSize + 1.0)
        }
    
        clusterCount * Math.log(alpha) - sum
      }
 
      val rest = crp.Restaurant(clusters)
      rest.logPosteriorProbability shouldEqual logPosteriorProbability(p5)
    }
  }
}


