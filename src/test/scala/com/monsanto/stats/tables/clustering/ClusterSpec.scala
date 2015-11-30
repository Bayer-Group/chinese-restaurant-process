package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables.UnitSpec
import org.scalatest._
import Inspectors._
import com.monsanto.stats.tables._

class ClusterSpec extends UnitSpec {

  val p3 = ModelParams(3, 1, 1)
  val p5 = ModelParams(5, 1, 1)
  val p10 = ModelParams(10, 1, 1)
  val allTopicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
  val crp = new CRP(p5, allTopicVectorResults)
  val allTopicVectors: Vector[crp.TopicVector] = allTopicVectorResults.map(crp.TopicVector.from(_))
  val occ = allTopicVectors(0)
  val rest = allTopicVectors.tail
  val firstOne = allTopicVectors.take(1)
  val firstThree = allTopicVectors.take(3)

  "A Cluster" can {
    "contain 0 to many topic vectors, which it correctly reports via its size method" in {
      crp.Cluster(Vector.empty).size should equal (0)
      crp.Cluster(Vector(allTopicVectors(0))).size should equal (1)
      crp.Cluster(allTopicVectors.take(3)).size should equal (3)
      crp.Cluster(allTopicVectors).size should equal (allTopicVectors.size)
    }
  }
  it must {
    "enforce that all topic vectors have a given size" in {
      cancelIfRequirementElided()
      val crp3 = new CRP(p3, allTopicVectorResults)
      an [IllegalArgumentException] should be thrownBy {
        crp3.Cluster(allTopicVectorResults.map(crp3.TopicVector.from(_)))
      }
    }
    "offer a topic count sums map that gives the total of all people at the table's topic counts" in {
      val crpE = new CRP(p5, Vector.empty)
      crpE.Cluster(Vector.empty).topicCountsSums shouldEqual crp.VecMap(Map.empty)
      val crp2 = new CRP(p5, allTopicVectorResults)
      crp2.Cluster(Vector(crp2.TopicVector.from(allTopicVectorResults(0)))).topicCountsSums shouldEqual crp2.VecMap(allTopicVectorResults(0).vecMap)
/*    TODO later bill
      val firstThree = data.take(3)
      info(s"firstThree was $firstThree")
      crp.Cluster(firstThree).topicCountsSums shouldEqual Map.empty
*/
    }
/*
    "calculate a probability vector" in { pending
      val data: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, data)
      crp.Cluster(Vector.empty).probabilityVector.vecMap should equal (Map.empty)
      crp.Cluster(Vector.empty).probabilityVector.vecMap should equal (Map.empty)
      val first: crp.TopicVector = data(0)
      def probVec(tv: TopicVector): ProbabilityVector = {
        val sum: Double = tv.vecMap.values.sum.toDouble
        val vecMap = first.vecMap.mapValues(v => v / sum)
        ProbabilityVector(TopicVector.Small, vecMap)
      }
      crp.Cluster(Vector(first)).probabilityVector.vecMap shouldEqual probVec(first)
/*
      crp.Cluster(Vector(data(0))).probabilityVector should equal (1)
      crp.Cluster(data.take(3)).probabilityVector should equal (3)
      crp.Cluster(data).size probabilityVector equal (data.size)
*/
    }
*/
/*
    "allow an individual to be removed" in {
      val data: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, data)
      crp.Cluster(data.take(3).map(crp.TopicVector.from(_))).remove(2) shouldEqual crp.Cluster(data.take(2).map(crp.TopicVector.from(_)))
    }
*/
    "offer a way to calculate for an individual (not sitting at the table already) the relative probability they should site their conditional on the other tables" in { pending
      
    }
    "offer a method that indicates whether it contains just one occupant" in {
      val data: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, data)
      crp.Cluster(Vector.empty).hasOneOccupant shouldBe false
      crp.Cluster(data.take(1).map(crp.TopicVector.from(_))).hasOneOccupant shouldBe true
      crp.Cluster(data.take(3).map(crp.TopicVector.from(_))).hasOneOccupant shouldBe false
    }
    "offer a method that adds an occupant" in {
      val lastOcc = allTopicVectors.last
      info(s"occ: $occ")
      info(s"firstOne: $firstOne")
      crp.Cluster(Vector.empty).addOccupant(lastOcc).topicVectors shouldBe Vector(lastOcc)
      crp.Cluster(firstOne).addOccupant(lastOcc).topicVectors shouldBe lastOcc +: firstOne
      crp.Cluster(firstThree).addOccupant(lastOcc).topicVectors shouldBe lastOcc +: firstThree
      intercept[IllegalStateException] {
        crp.Cluster(firstOne).addOccupant(firstOne(0))
      }
    }
   
    "ensure topicCountSums is updated after adding an occupant" in {
      val data: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, data)
      val occ = crp.TopicVector.from(data(0))
      val rest = data.tail.map(crp.TopicVector.from(_))
      val firstOne = rest.take(1)
      val emptyCluster = crp.Cluster(Vector.empty)
      val clusterWithOcc = emptyCluster.addOccupant(occ)
      clusterWithOcc.topicCountsSums should not equal emptyCluster.topicCountsSums
    }

    "offer a method that removes an occupant, but throws ISE if it contains one or zero occupants" in {
      val topicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, topicVectorResults)
      val data = topicVectorResults.map(crp.TopicVector.from(_))
      val occ = data(0)
      val firstOne = data.take(1)
      val firstThree = data.take(3)
      info(s"occ: $occ")
      info(s"firstOne: $firstOne")
      intercept[IllegalStateException] {
        crp.Cluster(Vector.empty).removeOccupant(occ.id)
      }
      intercept[IllegalStateException] {
        crp.Cluster(firstOne).removeOccupant(occ.id)
      }
      crp.Cluster(firstThree).removeOccupant(data(0).id) shouldEqual crp.Cluster(data.tail.take(2))
      intercept[IllegalArgumentException] {
        crp.Cluster(firstThree.drop(1)).removeOccupant(occ.id)
      }
    }
    "offer a cValue that uses estimateCSmoothingFirst that returns the same value as the original version" in {

      val topicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
      val params = ModelParams(5, 1, 0.01)
      val crp = new CRP(params, topicVectorResults)
      val topicVectors: Vector[crp.TopicVector] = topicVectorResults.map(crp.TopicVector.from(_))

      def yeOldeEstimateC(params: ModelParams, vecMap: Map[Int, Double]): Double = {

        // Sums up the Ai and the LogGammaAi for non-small entries
        val (partSumAi, partSumLogGammaAi) = vecMap.foldLeft((0.0, 0.0)) { case ((sumAi: Double, sumLogGammaAi: Double), (_: Int, v: Double)) =>
          (sumAi + v, sumLogGammaAi + crp.logGamma(v))
        }

        val countOfSmalls: Int = params.topicVectorSize - vecMap.size
        val sumOfSmalls: Double = countOfSmalls * params.beta
        val totalSumAi: Double = partSumAi + sumOfSmalls

        val logGammaOfSmalls: Double = countOfSmalls * crp.logGamma(params.beta)
        val totalLogGammaAi: Double = partSumLogGammaAi + logGammaOfSmalls

        totalLogGammaAi - crp.logGamma(totalSumAi)
      }
      def yeOldeSmoothedCounts(beta: Double, vecMap: Map[Int, Int]): Map[Int, Double] = vecMap.mapValues(_ + beta)
      def yeOldeCValue(params: ModelParams, vecMap: Map[Int, Double]): Double = yeOldeEstimateC(params, vecMap)

      val smallClusters: Vector[crp.Cluster] = topicVectors map { tv => crp.Cluster(Vector(tv)) }
      val bigCluster: crp.Cluster = crp.Cluster(topicVectors)
      val clusters: Vector[crp.Cluster] = bigCluster +: smallClusters
      forEvery (clusters) { cl =>
        val localSmoothedCounts: Map[Int, Double] = yeOldeSmoothedCounts(params.beta, cl.topicCountsSums.toMap)
        val localCValue: Double = yeOldeCValue(params, localSmoothedCounts)
        localCValue shouldEqual (cl.cValue +- 0.000000000001)
      }
    }
    "offer an imperative (fast) posteriorLogPdf method that returns the same value as the original higher-order method version" in {

      val topicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
      val crp = new CRP(p5, topicVectorResults)
      val params = ModelParams(5, 1, 0.01)
      val topicVectors: Vector[crp.TopicVector] = topicVectorResults.map(crp.TopicVector.from(_))

      def yeOldePosteriorLogPdf(tv: crp.TopicVector, vecMap: Map[Int, Int], cValue: Double): Double = {
        // Add tv's vecMap to smoothedCounts vecMap. // MAKE THIS SIMPLER
        // First get a value for every single thing, and the value is a Double. If not in this vecmap, then you'll get the beta value.
        val temp: Vector[(Int, Int)] = tv.vecMap.toMap.toVector ++ vecMap.toVector
        // This one groups it by the first one, which is the index, which will be 1 if it didn't exist here, and 2 elements if it does.
        val temp2: Map[Int, Vector[(Int, Int)]] = temp.groupBy(_._1)
        // This one gets rid of the index in the value, so just have the Doubles
        val temp3: Map[Int, Vector[Int]] = temp2.mapValues(xs => xs.map(_._2))
        // This one sums them up.
        val topicCountsSums: Map[Int, Int] = temp3.mapValues(_.sum)
        crp.VecMap(topicCountsSums).estimateCSmoothingFirst - cValue
      }

      val cluster = crp.Cluster(topicVectors)
      val cValue: Double = cluster.topicCountsSums.estimateCSmoothingFirst
      val yeOldies =
        cluster.topicVectors.map { tv =>
          yeOldePosteriorLogPdf(tv, cluster.topicCountsSums.toMap, cValue)
        }
      val shinyNews =
        cluster.topicVectors.map { tv =>
          cluster.posteriorLogPdf(tv)
        }
      val pairs = yeOldies zip shinyNews
      forAll (pairs) { case (shinyNew, yeOlde) => shinyNew shouldEqual yeOlde +- 0.0000000001 }
    }
  }
}

