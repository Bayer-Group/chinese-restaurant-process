
package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables.UnitSpec
import org.scalatest._
import enablers.Collecting
import scala.collection.GenTraversable
import prop.GeneratorDrivenPropertyChecks
import org.scalactic.Equality
import org.scalactic.anyvals.{ PosZInt, PosZDouble }
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import scala.collection.mutable
import com.monsanto.stats.tables._

class CRPSpec extends UnitSpec with Inspectors with GeneratorDrivenPropertyChecks {

  val allTopicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
  val p5 = ModelParams(5, 1, 1)
  val crp = new CRP(p5, allTopicVectorResults)
  val allTopicVectors: Vector[crp.TopicVector] = allTopicVectorResults.map(crp.TopicVector.from(_))
  val occ = allTopicVectorResults(0)
  val rest = allTopicVectorResults.tail
  val firstOne = rest.take(1)
  val firstThree = rest.take(3)

  val posDoubles = for (n <- Gen.choose(0.0, Double.MaxValue)) yield n

  val posInts = for (n <- Gen.choose(0, Int.MaxValue)) yield n

  val posZIntGen: Gen[PosZInt] =
    for { i <- choose(0, Int.MaxValue) } yield PosZInt.from(i).get
  implicit val arbPosZInt: Arbitrary[PosZInt] = Arbitrary(posZIntGen)
 
  val posZDoubleGen: Gen[PosZDouble] =
    for { d <- choose(0, Double.MaxValue) } yield PosZDouble.from(d).get
  implicit val arbPosZDouble: Arbitrary[PosZDouble] = Arbitrary(posZDoubleGen)
 
  val doubleEqualityIncludingNaN: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: Double) if a.isNaN && bDouble.isNaN  => true
          case _ => a == b
        }
    }

  "CRP.initializeClusters" should {
    "create one cluster for every topic vector" in { 
      val clusters: Vector[crp.Cluster] = crp.initializeClusters()
      allTopicVectorResults.length shouldEqual clusters.length
    }
    "place each topic vector in its own cluster in random order" in { 

      val clusters: Vector[crp.Cluster] = crp.initializeClusters()

      val topicVectorIds = allTopicVectorResults.map(tv => (tv.id, tv)).map { case (id, _) => id }
      val clusterIds = clusters.map(c => (c.topicVectors(0).id, c)).map { case (id, _) => id }
      // Assert they are not in the same order by saying at least one should be different. Teeny tiny
      // chance this could flicker if we hit the jackpot.
      forAtLeast (1, clusterIds zip topicVectorIds) { case (x, y) => x should not equal y }
    }
    "throw IAE if a passed topic vector has a size greater than the passed topicVectorSize" in { 
      cancelIfRequirementElided()
      val p3 = ModelParams(3, 1, 1)
      val crp = new CRP(p3, allTopicVectorResults)
      an [IllegalArgumentException] should be thrownBy {
        crp.initializeClusters()
      }
    }
    "shuffle by default, but allow a flag to be passed down to disable shuffling, for repeatable performance tests" in {
      val clusters1: Vector[crp.Cluster] = crp.initializeClusters(false)
      val clusters2: Vector[crp.Cluster] = crp.initializeClusters(false)
      clusters1 shouldEqual clusters2
      val clusters3: Vector[crp.Cluster] = crp.initializeClusters()
      clusters3 should not equal clusters2 // Small chance of flicker here, because shuffle could come out the same
    }
  }
  "CRP.extract" should {
    "reject negative and too-large indexes with IAE" in {
      intercept[IllegalArgumentException] {
        crp.extract(-1, crp.Restaurant(Vector.empty))
      }
      intercept[IllegalArgumentException] {
        val crp = new CRP(p5, Vector.empty)
        crp.extract(1, crp.Restaurant(Vector.empty))
      }
    }
    "extract the topic vector at the given index and remove the cluster if there are no other occupants" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val e = crp.extract(0, r)
      e.extractedIdx shouldEqual 0
      e.extracted shouldEqual allTopicVectors(0)
      e.clusters.size shouldEqual r.clusters.size - 1
      val topicVectorsInClusters = for (c <- e.restaurant.clusters; tv <- c.topicVectors) yield tv
      topicVectorsInClusters.size shouldEqual allTopicVectorResults.size - 1
    }
    "extract the topic vector at the given index and remove the topic vector from the cluster if there are remaining occupants" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val e = crp.extract(0, r)
      // Add the extracted topic back to the now 0th cluster
      val newFirstCluster = e.restaurant.clusters(0).addOccupant(e.extracted)
      val r2 = crp.Restaurant(e.restaurant.clusters.updated(0, newFirstCluster))
      val e2 = crp.extract(0, r2)
      e2.extractedIdx shouldEqual 0
      e2.extracted shouldEqual allTopicVectors(0)
      e2.clusters.size shouldEqual r2.clusters.size
      val topicVectorsInClusters = for (c <- e2.restaurant.clusters; tv <- c.topicVectors) yield tv
      topicVectorsInClusters.size shouldEqual allTopicVectorResults.size - 1
    }
  }
  "CRP.insert" should {
    "reject negative and too-large indexes with IAE" in {
      intercept[IllegalArgumentException] {
        crp.insert(Some(-1), crp.Extraction(crp.TopicVector.from(occ), 0, crp.Restaurant(Vector.empty)))
      }
      intercept[IllegalArgumentException] {
        crp.insert(Some(1), crp.Extraction(crp.TopicVector.from(occ), 0, crp.Restaurant(Vector.empty)))
      }
    }
    "insert the topic vector in a brand new cluster if clusterIdxOpt is None" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val e = crp.extract(0, r)
      e.restaurant.clusters.size shouldEqual r.clusters.size - 1 // sanity check
      val r2 = crp.insert(None, e) // insert it back in its own cluster
      r2.clusters.size shouldEqual r.clusters.size
      // Ensure assignments is also updated correctly
      r2.assignment(0) shouldEqual r2.clusters.length - 1
    }
    "insert the topic vector in an existing cluster if clusterIdxOpt is a Some" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val e = crp.extract(0, r)
      e.restaurant.clusters.size shouldEqual r.clusters.size - 1 // sanity check
      val r2 = crp.insert(Some(0), e) // insert it back into the first cluster
      r2.clusters.size shouldEqual r.clusters.size - 1
      r2.clusters(0).topicVectors.size shouldBe 2
      r2.clusters(0).topicVectors should contain (e.extracted)
      // Ensure assignments is also updated correctly
      r2.assignment(0) shouldEqual 0
    }
  }
  "CRP.reseatAllCustomers" should {
    "produce a restaurant with all topics at one table if given a selector that always selects cluster 0" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val r2 = crp.reseatAllCustomers(r, RealRandomNumGen, (e, r) => Some(0))
      r2.clusters.size shouldEqual 1
      r2.clusters(0).topicVectors.size shouldEqual allTopicVectorResults.size
    }
    "produce a restaurant with each topics at its own table if given a selector that always selects a new cluster" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val r2 = crp.reseatAllCustomers(r, RealRandomNumGen, (e, r) => None)
      r2.clusters.size shouldEqual r.clusters.size
      forAll (r2.clusters) { case crp.Cluster(topicVectors, _) => topicVectors.size shouldEqual 1 }
    }
  }
  "CRP.weightedProbabilitySampling" should {
    "return the same value as the imperative version" in {
      def imperativeWeightedProbabilitySampling(probabilityTable: Array[Double], rng: RandomNumGen): Int = {
        val randomNumber0To1 = rng.next()
        var sampledId: Int = 0

        // This value is used to determine the outcome of the random assignment.
        // This is the analogous of having a line of length 1, separate the line
        // sublines of length equal to their probability, selecting a random
        // number which is the position on this line and then start summing up
        // the positions until you hit the point where subline where the random
        // number lies.
        var probabilitySumSelector: Double = 0.0

        // loop through all the table and find their weight then see if the
        // random number lies.
        var i = 0
        var found = false
        while (i < probabilityTable.length && !found) {
          probabilitySumSelector += probabilityTable(i)
          if (randomNumber0To1 <= probabilitySumSelector) {
            sampledId = i
            found = true
          }
          i += 1
        }
 
        sampledId
      }
      val rng1: RandomNumGen = MockRandomNumGen(Vector(0.23, 1.0, 0.3, 0.88))
      val rng2: RandomNumGen = MockRandomNumGen(Vector(0.23, 1.0, 0.3, 0.88))
      forAll { (vec: Array[Double]) => 
        crp.weightedProbabilitySampling(vec, rng1) shouldEqual imperativeWeightedProbabilitySampling(vec, rng2)
      }
    }
  }
  "new CRP.LogGamma" should {
    "return the same value as the imperative version" in {
      def retranslatedLogGamma(x: Double): Double = {

        val tmp: Double = (x - 0.5) * Math.log(x + 4.5) - (x + 4.5);

        val ser: Double = 1.0 + 76.18009173    / (x + 0)   - 86.50532033    / (x + 1) +
                        24.01409822    / (x + 2)   -  1.231739516   / (x + 3) +
                        0.00120858003 / (x + 4)   -  0.00000536382 / (x + 5);

        tmp + Math.log(ser * Math.sqrt(2 * Math.PI))
      }

      implicit val doubleEquality: Equality[Double] = doubleEqualityIncludingNaN

      forAll { (x: Double) => 
        crp.logGamma(x) shouldEqual retranslatedLogGamma(x)
      }
    }
  }
  "CRP.estimateCSmoothingFirst" should {
    "return the same value as a different version" in {


      def deepBlueC(params: ModelParams, vecMap: Map[Int, Int]): Double = {

        var sumAi: Double = 0.0
        var sumLogGammaAi: Double = 0.0

        val vecVec: Vector[(Int, Int)] = vecMap.toVector
        var i = 0
        while (i < params.topicVectorSize) {
          val tmp: Double = if (i < vecVec.length) (vecVec(i)._2 + params.beta) else params.beta
          sumAi += tmp
          sumLogGammaAi += crp.logGamma(tmp)
          i += 1
        }

        sumLogGammaAi - crp.logGamma(sumAi)
      }

      implicit val doubleEquality: Equality[Double] = doubleEqualityIncludingNaN

      forAll { (zVecMap: Map[PosZInt, PosZInt]) => 
        val vecMap: Map[Int, Int] = zVecMap map { case (i, j) => (i.toInt % p5.topicVectorSize -> j.toInt % p5.topicVectorSize) }
        crp.VecMap(vecMap).estimateCSmoothingFirst shouldEqual deepBlueC(p5, vecMap) +- 0.0000000000001
      }
    }
  }

  "CRP.clusterWeights" should {
    "return the same value as the imperative version" in {

      def clusterProbabilities(params: ModelParams, tv: crp.TopicVector, clusters: Vector[crp.Cluster]): Vector[Double] = {

        val totalClusters: Int = clusters.size
        var condProbCiGivenXiAndOtherCi: Array[Double] = new Array[Double](totalClusters)

        // Probabilities that appear on https://www.cs.cmu.edu/~kbe/dp_tutorial.posteriorLogPdf
        // Calculate the probabilities of assigning the point for every cluster
        var k = 0
        while (k < totalClusters) {
          val ck: crp.Cluster = clusters(k)
          val marginalLogLikelihoodXi: Double = ck.posteriorLogPdf(tv)
          val mixingXi: Double = ck.size / (params.alpha + allTopicVectorResults.length - 1)

          condProbCiGivenXiAndOtherCi(k) = marginalLogLikelihoodXi + Math.log(mixingXi)
          k += 1
        }
        
        condProbCiGivenXiAndOtherCi.toVector
      }

      val r = crp.Restaurant(crp.initializeClusters())
      for (tvi <- 0 until allTopicVectorResults.length) { 
        val e = crp.extract(tvi, r)
        val xs = crp.clusterWeights(e.extracted, e.restaurant.clusters).toVector.init
        val ys = clusterProbabilities(p5, e.extracted, e.restaurant.clusters)
        xs.length shouldEqual ys.length
        forEvery (xs zip ys) { case (x, y) => x shouldEqual y }
      }
    }
  }
  "CRP.parClusterWeights" should {
    "return the same value as the sequential version" in {

      // Lying here a bit by saying 10000 and 100000 but actually using 5 and 365, but we get a failing test
      // before I fixed this bug, and a passing test after:
      // - val mixingTv = c.size / (params.alpha + params.totalTopics - 1)      // double mixingXi = ck.size()/(alpha+n-1);
      // + val mixingTv = c.size / (params.alpha + params.totalItemsToCluster - 1)      // double mixingXi = ck.size()/(alpha+n-1);
      val p10k = ModelParams(10000, 1, 1)
      val crp = new CRP(p10k, allTopicVectorResults)
      val r = crp.Restaurant(crp.initializeClusters())
      for (tvi <- 0 until allTopicVectorResults.length) { 
        val e = crp.extract(tvi, r)
        val xs = crp.parClusterWeights(e.extracted, e.restaurant.clusters.toVector)
        val ys = crp.clusterWeights(e.extracted, e.restaurant.clusters.toVector).toVector.init
        xs.length shouldEqual ys.length
        forEvery (xs zip ys) { case (x, y) => x shouldEqual y }
      }
    }
  }
  "CRP.tableAssignmentProbabilities" should {
    "return the same value as the imperative version" is pending
  }
  "CRP.parTableAssignmentProbabilities" should {
    "return the same value as the sequential version" in {
      val r = crp.Restaurant(crp.initializeClusters())
      val e = crp.extract(0, r)
      val parallel = crp.parTableAssignmentProbabilities(e.extractedIdx, e.extracted, e.restaurant.clusters)
      val sequential = crp.tableAssignmentProbabilities(e.extractedIdx, e.extracted, e.restaurant.clusters) 
      // The tolerance is necessary right now. Might be a sign of a bug, or maybe just a sign that order
      // matters and we aren't doing things in order in parallel?
      forAll (parallel zip sequential) { case (p, s) => p shouldEqual s +- 0.000000000000001 }
    }
  }

  "CRP.selectCluster" should {
    "find the types of MnM bags" ignore {
      val cannedAllTopicVectorResults: Vector[TopicVectorInput] = MnMGen.cannedData
      val cannedCrp = new CRP(ModelParams(5, .05, .05), cannedAllTopicVectorResults)
      val crpResult = cannedCrp.findClusters(200, RealRandomNumGen, cannedCrp.selectCluster)
      crpResult.clusters.size shouldBe 3
    }
  }
}
    
