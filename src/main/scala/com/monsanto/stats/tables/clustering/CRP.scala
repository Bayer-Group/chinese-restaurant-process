package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables.utils.ProfileUtils

import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.annotation.elidable
import elidable._
import ProfileUtils.time
import scala.annotation.tailrec

class CRP(params: ModelParams, allTopicVectorResults: Vector[TopicVectorInput]) { thisCRP =>

  private val cache: Array[Double] = new Array[Double](allTopicVectorResults.length)

  {
    var idx: Int = 0
    while (idx < allTopicVectorResults.length) {
      cache(idx) = logGamma(idx.toDouble + params.beta)
      idx += 1
    }
  }

  // Array(key0, value0, key1, value1, key2, value2, ... plus possibly some unused elements at the end)
  final class VecMap private (private val pairs: Array[Int], val size: Int) {

    // Ensure array length is even
    // require((pairs.length & 1) == 0)
    // require(pairs.length >= size) // Pairs can have extra slots
  
    def toMap: Map[Int, Int] = {
     var result = Map.empty[Int, Int]
     val doubleSize = size * 2
     var idx = 0
     while (idx < doubleSize) {
       result += pairs(idx) -> pairs(idx + 1)
       idx += 2
     }
     result
    }
  
    def +(that: VecMap): VecMap = {
      val thisLen = this.size * 2 // Length of used portion of this.pairs array
      val thatLen = that.size * 2 // Length of used portion of that.pairs array
      val newPairs: Array[Int] = new Array[Int](thisLen + thatLen)
      var thisIdx = 0
      var thatIdx = 0
      var newIdx = 0
      while (thisIdx < thisLen && thatIdx < thatLen) {
        val thisKey = this.pairs(thisIdx)
        val thatKey = that.pairs(thatIdx)
        if (thisKey == thatKey) {
          newPairs(newIdx) = thisKey
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1) + that.pairs(thatIdx + 1)
          thisIdx += 2
          thatIdx += 2
        }
        else if (thisKey < thatKey) {
          newPairs(newIdx) = thisKey
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1)
          thisIdx += 2
        }
        else {
          newPairs(newIdx) = thatKey
          newPairs(newIdx + 1) = that.pairs(thatIdx + 1)
          thatIdx += 2
        }
        newIdx += 2
      }
      if (thisIdx < thisLen) {
        // that.pairs is spent. Just finish off this
        while (thisIdx < thisLen) {
          newPairs(newIdx) = this.pairs(thisIdx)
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1)
          thisIdx += 2
          newIdx += 2
        }
      }
      else if (thatIdx < thatLen) {
        // this.pairs is spent. Just finish off that
        while (thatIdx < thatLen) {
          newPairs(newIdx) = that.pairs(thatIdx)
          newPairs(newIdx + 1) = that.pairs(thatIdx + 1)
          thatIdx += 2
          newIdx += 2
        }
      }
      assert((newIdx & 1) == 0)
      new VecMap(newPairs, newIdx / 2)
    }

    def -(that: VecMap): VecMap = {
      val thisLen = this.size * 2 // Length of used portion of this.pairs array
      val thatLen = that.size * 2 // Length of used portion of that.pairs array
      val newPairs: Array[Int] = new Array[Int](thisLen)
      var thisIdx = 0
      var thatIdx = 0
      var newIdx = 0
      while (thisIdx < thisLen && thatIdx < thatLen) {
        val thisKey = this.pairs(thisIdx)
        val thatKey = that.pairs(thatIdx)
        if (thisKey == thatKey) {
          newPairs(newIdx) = thisKey
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1) - that.pairs(thatIdx + 1)
          thisIdx += 2
          thatIdx += 2
          newIdx += 2
        }
        else if (thisKey < thatKey) {
          newPairs(newIdx) = thisKey
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1)
          thisIdx += 2
          newIdx += 2
        }
        else {
          // Leave this one out entirely, and don't incremenet newIdx
          thatIdx += 2
        }
      }
      if (thisIdx < thisLen) {
        // that.pairs is spent. Just finish off this
        while (thisIdx < thisLen) {
          newPairs(newIdx) = this.pairs(thisIdx)
          newPairs(newIdx + 1) = this.pairs(thisIdx + 1)
          thisIdx += 2
          newIdx += 2
        }
      } // else this is spent, so we're don
      assert((newIdx & 1) == 0)
      new VecMap(newPairs, newIdx / 2)
    }
  
    override def equals(other: Any): Boolean = {
      other match {
        case that: VecMap => this.pairs.take(this.size * 2).deep == that.pairs.take(that.size * 2).deep
        case _ => false
      }
    }
  
    override def hashCode: Int = java.util.Arrays.hashCode(pairs)
  
    override def toString: String = {
      val eleStrs = toMap map { case (k, v) => s"$k -> $v" }
      eleStrs.mkString("VecMap(", ", ", ")")
    }

    /*
     * C is just the result of this integral. C tells you the proability that someone is going to sit somewhere and
     * the probability of your uncertainty about what the parameters of that table truly are. If you toss 10 coins and
     * get 6 heads 4 tails, you'd guess it is 60/40, but you wouldn't be very certain. If you had 1000 samples you'd be
     * more certain, and likely be closer to 50/50. C it is accounting for that uncertainty.
    */
    def estimateCSmoothingFirst: Double = {
  
      // Compute partSumAi and partSumLogGammaAi by iterating through all
      // values in the WeightedVector's vecMap and computing the sum of the
      // values and their logGammas.
      var partSumAi: Double = 0.0
      var partSumLogGammaAi: Double = 0.0
      var idx = 1
      val len = size * 2
      while (idx < len) {
        val v = pairs(idx)
        partSumAi += v + params.beta // add beta to this and the next value to smooth the curve
        val logGammaSmoothingFirst =
          if (v < allTopicVectorResults.length) cache(v)
          else logGamma(v + params.beta)
        partSumLogGammaAi += logGammaSmoothingFirst
        idx += 2
      }
  
      // The number of "smalls" (used in smoothing) is going to be the topic vector size minus the size
      // of the weighted vector, because our weighted vectors are sparse (zeros are left out). When we
      // "smooth," we change those zeros into small numbers.
      val countOfSmalls: Int = params.topicVectorSize - size
  
      // Because wv is a sparse vector, it does not contain zero values. To smooth, we want to add params.beta
      // to each conceptual value, including the zeros. So each zero should become params.beta. Then we'll want
      // to sum them all up. The sum of just these small numbers will be the countOfSmalls * params.beta:
      val sumOfSmalls: Double = countOfSmalls * params.beta
  
      // The total of the unlogged values is that partSumAi, which is the sum of all the non-zero values
      // (which existed in the wv sparse vector) plus the sumOfSmalls calculated above.
      val totalSumAi: Double = partSumAi + sumOfSmalls
  
      // Similarly, the logGammaOfSmalls is the logGamma of params.beta multiplied by the number of zero values,
      // which are left out of the wv sparse vector.
      val logGammaOfSmalls: Double = countOfSmalls * logGamma(params.beta)
  
      // Similar to above, the total log gamma sum is the one computed from the non-zero values that appeared
      // in the sparse vector plus the logGammaOfSmalls, computed above.
      val totalLogGammaAi: Double = partSumLogGammaAi + logGammaOfSmalls
  
      // Result is the total log gamma sum minus the logGamma of the total regular sum
      totalLogGammaAi - logGamma(totalSumAi)
    }
  }
  
  object VecMap {
  
    // Array(key0, value0, key1, value1, key2, value2, ...)
    def apply(map: Map[Int, Int]): VecMap = {
      val sorted: Vector[(Int, Int)] = map.toVector.sortBy(_._1)
      val pairs = new Array[Int](sorted.size * 2)
      var it = sorted.iterator
      var idx = 0
      while (it.hasNext) {
        val (k, v) = it.next()
        pairs(idx) = k
        pairs(idx + 1) = v
        idx += 2
      }
      new VecMap(pairs, sorted.size)
    }
  }

  case class TopicVector(id: Long, vecMap: VecMap) {
    def toTopicVectorResult: TopicVectorInput = TopicVectorInput(id, vecMap.toMap)
  }

  object TopicVector {
    def from(tvr: TopicVectorInput): TopicVector = TopicVector(tvr.id, VecMap(tvr.vecMap))
  }

  val allTopicVectors: Vector[TopicVector] = allTopicVectorResults.map(tvr => TopicVector(tvr.id, VecMap(tvr.vecMap)))

  case class Cluster(topicVectors: Vector[TopicVector], topicCountsSums: VecMap) {

    Cluster.requireTopicVectorsSizeLessThanOrEqualToTotal(topicVectors, params.topicVectorSize) // This is #3

    def size: Int = topicVectors.size
    val cValue: Double = topicCountsSums.estimateCSmoothingFirst
    def posteriorLogPdf(tv: TopicVector): Double = {
      val newTopicCountsSums: VecMap = tv.vecMap + topicCountsSums
      // Now newTopicCountsSums is a sparse vector vecMap that represents the sum of the tv.vecMap
      // and the topicCountSums with no smoothing yet.
      newTopicCountsSums.estimateCSmoothingFirst - cValue
    }
    def hasOneOccupant: Boolean = topicVectors.length == 1
    def addOccupant(topicVector: TopicVector): Cluster = {
      if (topicVectors.exists(_.id == topicVector.id)) { // Now this if check is showing up #2 anon
        throw new IllegalStateException
      }
      val newTopicVectors = topicVector +: topicVectors
      val newTopicCountsSums = topicVector.vecMap + this.topicCountsSums
  
      copy(topicVectors = newTopicVectors, topicCountsSums = newTopicCountsSums)
    }
    def removeOccupant(topicId: Long): Cluster = {
      if (topicVectors.length <= 1) { // TODO: requireState 
        throw new IllegalStateException
      }
      val topicIdx = topicVectors.indexWhere(tv => tv.id == topicId) // Now this one is up there, #1 anon
      if (topicIdx == -1) {
        throw new IllegalArgumentException("topicId not found")
      }
      val tvToRemove = topicVectors(topicIdx)
      Cluster(topicVectors.take(topicIdx) ++ topicVectors.drop(topicIdx + 1), topicCountsSums - tvToRemove.vecMap)
    }
  }
  
  object Cluster {
    def apply(topicVectors: Vector[TopicVector]): Cluster = {
      val topicCountsSums: VecMap = computeTopicCountsSums(topicVectors)
      apply(topicVectors, topicCountsSums)
    }
 // This is # 1 right now
    def computeTopicCountsSums(topicVectors: Vector[TopicVector]): VecMap = {
      var idx = 0
      var sum = VecMap(Map.empty)
      while (idx < topicVectors.length) {
        val vecMap = topicVectors(idx).vecMap
        sum += vecMap
        idx += 1
      }
      sum
    }
  
    @elidable(ASSERTION) def requireTopicVectorsSizeLessThanOrEqualToTotal(topicVectors: Vector[TopicVector], topicVectorSize: Int): Unit = {
      if (topicVectors exists { case TopicVector(_, mp) => mp.size > topicVectorSize }) {
        val probs: Vector[(Long, Int)] = topicVectors.collect { case TopicVector(id, mp) if mp.size > topicVectorSize => (id, mp.size) }
        throw new IllegalArgumentException(s"${probs.size} topic vector maps had size > params.topicVectorSize $topicVectorSize: ${probs}")
      }
    }
  }

  case class Restaurant private (
    clusters: Vector[Cluster],
    private val assignments: Array[Int] // Array index is TopicVector idx; element is cluster idx
  ) { thisRestaurant =>
    def assignment(topicVectorIdx: Int): Int = assignments(topicVectorIdx)

    // Insert the topic vector
    def insert(topicVectorIdx: Int, optClusterIdx: Option[Int]): Restaurant = {
      optClusterIdx match {
        case None => // Put in new cluster at end
          val newAssignments = assignments.clone()
          newAssignments(topicVectorIdx) = clusters.length
          new Restaurant(
            clusters :+ Cluster(Vector(allTopicVectors(topicVectorIdx))),
            newAssignments // assignments + (topicVectorIdx -> clusters.length)
          )
        case Some(clusterIdx) =>
          val newCluster = clusters(clusterIdx).addOccupant(allTopicVectors(topicVectorIdx))
          val newClusters = clusters.updated(clusterIdx, newCluster)
          val newAssignments = assignments.clone()
          newAssignments(topicVectorIdx) = clusterIdx
          new Restaurant(
            newClusters,
            newAssignments // assignments + (topicVectorIdx -> clusterIdx)
          )
      }
    }
    def extract(topicVectorIdx: Int): Extraction = {
      require(topicVectorIdx >= 0 && topicVectorIdx < allTopicVectors.length)
      val topicVector = allTopicVectors(topicVectorIdx)
      val clusterIdx = thisRestaurant.assignment(topicVectorIdx)
      val cluster = thisRestaurant.clusters(clusterIdx)
      val newClusterOpt =
        if (cluster.hasOneOccupant) None
        else Some(cluster.removeOccupant(topicVector.id))
      val (newClusters, newAssignments) =
        newClusterOpt match {
          case None => // remove cluster
            val theNewClusters = thisRestaurant.clusters.take(clusterIdx) ++ thisRestaurant.clusters.drop(clusterIdx + 1)
            val theNewAssignments = assignments.clone()
            // val it: Iterator[(Int, Int)] = assignments.iterator
            var tvIdx = 0
            while (tvIdx < theNewAssignments.length) {
              val cIdx = theNewAssignments(tvIdx)
              if (cIdx == clusterIdx) { // drop the entry for the cluster we just removed
                 theNewAssignments(tvIdx) = -1
              }
              else if (cIdx > clusterIdx) {
                theNewAssignments(tvIdx) = cIdx - 1
              }
              tvIdx += 1
            }
            (theNewClusters, theNewAssignments)

          case Some(newCluster) =>
            val theNewClusters = thisRestaurant.clusters.updated(clusterIdx, newCluster)
            val theNewAssignments = assignments.clone()
            theNewAssignments(topicVectorIdx) = -1
            // val theNewAssignments = assignments - topicVectorIdx
            (theNewClusters, theNewAssignments)
        }
      val newRestaurant = new Restaurant(newClusters, newAssignments)
      Extraction(allTopicVectors(topicVectorIdx), topicVectorIdx, newRestaurant)
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: Restaurant => this.clusters == that.clusters
        case _ => false
      }
    }
 
    override def hashCode: Int = clusters.hashCode

    def dataLikelihoodIgnoringClusterCount: Double = {
      var idx = 0
      var sum: Double = 0.0
      while (idx < clusters.length) {
        sum += clusters(idx).cValue
        idx += 1
      }
      sum
    }

    // Returned array has key0, value0, key1, value1, key2, value2, ...
    def clusterSizeHistogram: Array[Int] = {
      val histoMap: Map[Int, Int] = clusters.groupBy(_.size).mapValues(_.size)
      var idx = 0
      val result = new Array[Int](2 * histoMap.size)
      val it = histoMap.iterator
      while (it.hasNext) {
        val (k, v) = it.next()
        result(idx) = k
        result(idx + 1) = v
        idx += 2
      }
      result
    }

    def clusterCountPart: Double =
      clusters.size * (logGamma(params.beta) * params.topicVectorSize - logGamma(params.beta * params.topicVectorSize))

    def logPosteriorProbability: Double = {
      val dLICC = dataLikelihoodIgnoringClusterCount
      val ccP = clusterCountPart
      val cA = Restaurant.logProbabilityOfClusteringArrangement(clusters.size, params.alpha, clusterSizeHistogram)

      (dLICC - ccP) + cA
    }

  }

  object Restaurant {
    def apply(clusters: Vector[Cluster]): Restaurant = {

      val assignments = new Array[Int](allTopicVectors.length)
      java.util.Arrays.fill(assignments, -1)

      var clusterIdx = 0
      while (clusterIdx < clusters.length) {
        val cluster = clusters(clusterIdx)
        var ctvIdx = 0
        while (ctvIdx < cluster.topicVectors.length) {
          val clustersTopicVector = cluster.topicVectors(ctvIdx)
          val topicVectorIdx = allTopicVectors.indexWhere(tv => tv.id == clustersTopicVector.id)
          assignments(topicVectorIdx) = clusterIdx
          ctvIdx += 1
        }
        clusterIdx += 1
      }

      new Restaurant(clusters, assignments)
    }

    def logProbabilityOfClusteringArrangement(
      clusterCount: Int,
      alpha: Double,
      clusterSizeHistogram: Array[Int] // key0, value0, key1, value1, ...
    ): Double = {
      var sum = 0.0
      var idx = 0
      while (idx < clusterSizeHistogram.length) {
        val clusterSize = clusterSizeHistogram(idx)
        val countOfThisSize = clusterSizeHistogram(idx + 1)
        sum += countOfThisSize * Math.log(clusterSize) + logGamma(countOfThisSize + 1.0)
        idx += 2
      }
      clusterCount * Math.log(alpha) - sum
    }
  }

  case class Extraction(
    extracted: TopicVector, 
    extractedIdx: Int,
    restaurant: Restaurant
  ) {
    val clusters: Vector[Cluster] = restaurant.clusters
    def assignment(topicVectorIdx: Int): Int = restaurant.assignment(topicVectorIdx)
  }

  def initializeClusters(shuffle: Boolean = true): Vector[Cluster] = {
    // Throw IAE if contained map has more elements than the passe topicVectorSize. 
    val unshuffled = allTopicVectors.map(tv => Cluster(Vector(tv))) 
    if (shuffle) Random.shuffle(unshuffled) else unshuffled
  }

  /*
   * LogGamma is a function that's in the Gibbs collapsed sampler. (In the uncollapsed sampler, what is the probability of
   * all the multinomials ... we have to estimate those parameters too. The collapsed sampler collapses all that into
   * one function, and that function happens to be expressed in the difference of sums in estimateC that uses
   * this log function. Gamma comes up a lot in multinomial stuff. Gamma is the continuous version of factorial.
   * The datumbox author did a Taylor series to 5 digits of precision and hard-coded the numbers.
   */
  def logGamma(x: Double): Double = {
    val tmp: Double = (x - 0.5) * Math.log(x + 4.5) - (x + 4.5)
    val ser: Double = 1.0 + 76.18009173 / (x + 0) - 86.50532033 / (x + 1) +
      24.01409822 / (x + 2) - 1.231739516 / (x + 3) +
      0.00120858003 / (x + 4) - 0.00000536382 / (x + 5)
    tmp + Math.log(ser * Math.sqrt(2 * Math.PI))
  }

  def parClusterWeights(tv: TopicVector, clustersMinusTv: Vector[Cluster]): ParVector[Double] = {
    clustersMinusTv.par.map { c =>
      val pdf = c.posteriorLogPdf(tv) // marginalLogLikelihoodXi 
      val mixingTv = c.size / (params.alpha + allTopicVectorResults.length - 1)      // double mixingXi = ck.size()/(alpha+n-1)
      pdf + Math.log(mixingTv)
    }
  }

  private final val ClusterWeightsDenom = params.alpha + allTopicVectorResults.length - 1

  /*
   * This method computes in one go for each topic vector (each customer, whose id is the key in the returned map), the probability
   * that they will be seated at a new cluster. One element at a time we'll add this to the end of the table assignment probabilities
   * vector using this line of code in tableAssignmentProbabilities:
   *
   *   val unnormalizedWeights: Vector[Double] = clusterWeights(params, tv, clustersMinusTv) :+ newClusterWeights(tv.id)
  */
  private final val cNew: Cluster = Cluster(Vector.empty)
  private final val probNewCluster: Double = params.alpha / ClusterWeightsDenom // This is the only one that changes
  private final val logProbNewCluster: Double = Math.log(probNewCluster)
  val newClusterWeights: Array[Double] = {
    val result = new Array[Double](allTopicVectors.length)
    var idx = 0
    while (idx < allTopicVectors.length) {
      val priorLogPredictive: Double = cNew.posteriorLogPdf(allTopicVectors(idx))
      result(idx) = (priorLogPredictive + logProbNewCluster)
      idx += 1
    }
    result
  }
  // Note: possible performance enhancement for weightsForStartingANewCluster: Could just leave out the priorLogPredictive here, and then
  // add it in later (in selectCluster) then we don't have to do it every time.

  // Result is one longer than the number of clusters, because last one is the probability we'll stick them in the new cluster

  /*
   * This method estimates the probability of assigning topic vector, tv, to the elements of the cluster vector (which
   * currently do not include tv, as he or she has been extracted.
  */
  def clusterWeights(tv: TopicVector, clustersMinusTv: Vector[Cluster]): Array[Double] = {
    val cmtLen = clustersMinusTv.length
    val result = new Array[Double](cmtLen + 1) // Room for one more at the end
    var idx = 0
    while (idx < cmtLen) {
      val c = clustersMinusTv(idx)
      val pdf = c.posteriorLogPdf(tv) // marginalLogLikelihoodXi 
      val mixingTv = c.size / ClusterWeightsDenom  // double mixingXi = ck.size()/(alpha+n-1)
      result(idx) = pdf + Math.log(mixingTv)
      idx += 1
    }
    result
  }

  def tableAssignmentProbabilities(tvIdx: Int, tv: TopicVector, clustersMinusTv: Vector[Cluster]): Array[Double] = {

    // Here is where I'm sticking the start new weight to the end!
    val unnormalizedWeights: Array[Double] = clusterWeights(tv, clustersMinusTv) // multiPurpose is first, unnormalizedWeights
    val uwLen = unnormalizedWeights.length
    unnormalizedWeights(uwLen - 1) = newClusterWeights(tvIdx)

    val maxWeight: Double = unnormalizedWeights.max
    val unloggedMaxRemovedWeights = unnormalizedWeights
    var idx = 0
    while (idx < uwLen) {
      val uw = unnormalizedWeights(idx)
      unloggedMaxRemovedWeights(idx) = Math.exp(uw - maxWeight)
      idx += 1
    }

    val sum: Double = unloggedMaxRemovedWeights.sum
    val result = unloggedMaxRemovedWeights
    
    idx = 0
    while (idx < uwLen) {
      val umrw = unloggedMaxRemovedWeights(idx)
      result(idx) = umrw / sum
      idx += 1
    }
    result
  }

  // Result is one longer than the number of clusters, because last one is the probability we'll stick them in the new cluster
  def parTableAssignmentProbabilities(tvIdx: Int, tv: TopicVector, clustersMinusTv: Vector[Cluster]): Vector[Double] = {
    // Here is where I'm sticking the start new weight to the end!
    val unnormalizedWeights: ParVector[Double] = parClusterWeights(tv, clustersMinusTv) :+ newClusterWeights(tvIdx)
    val maxWeight: Double = unnormalizedWeights.max
    val unloggedMaxRemovedWeights: ParVector[Double] = unnormalizedWeights.map(uw => Math.exp(uw - maxWeight))
    val sum: Double = unloggedMaxRemovedWeights.sum
    unloggedMaxRemovedWeights.map(uw => uw / sum).seq // might have to avoid divide by zero as in JAva version
  }

  def weightedProbabilitySampling(choices: Array[Double], rng: RandomNumGen): Int = {
    val randomNumber0To1: Double = rng.next()
    @tailrec
    def recurseWPS(sum: Double, idx: Int): Int = {
      if (idx == choices.length) 0
      else {
        val newSum = sum + choices(idx)
        if (newSum > randomNumber0To1) idx
        else recurseWPS(newSum, idx + 1)
      }
    }
    recurseWPS(0.0, 0)
  }

  // The parallel version has already boxed it, so take a Vector[Double]
  def weightedProbabilitySamplingForPar(choices: Vector[Double], rng: RandomNumGen): Int = {
    val randomNumber0To1: Double = rng.next()
    @tailrec
    def recurseWPS(sum: Double, idx: Int): Int = {
      if (idx == choices.length) 0
      else {
        val newSum = sum + choices(idx)
        if (newSum > randomNumber0To1) idx
        else recurseWPS(newSum, idx + 1)
      }
    }
    recurseWPS(0.0, 0)
  }

  def extract(topicVectorIdx: Int, restaurant: Restaurant): Extraction = restaurant.extract(topicVectorIdx)

  def insert(optClusterIdx: Option[Int], extraction: Extraction): Restaurant = {
    
    val requirementSatisfied =
      optClusterIdx match {
        case None => true
        case Some(clusterIdx) =>
          clusterIdx >= 0 && clusterIdx < extraction.restaurant.clusters.length 
      }
    require(requirementSatisfied, s"optClusterIdx was: $optClusterIdx")

    extraction.restaurant.insert(extraction.extractedIdx, optClusterIdx)
  }

  def reseatAllCustomers(
    restaurant: Restaurant,
    rng: RandomNumGen,
    selector: (Extraction, RandomNumGen) => Option[Int]
  ): Restaurant = {
    @tailrec
    def recurseRAC(topicVectorIdx: Int, rst: Restaurant): Restaurant = {
      if (topicVectorIdx >= allTopicVectors.length) rst
      else {
        val extraction =
            extract(topicVectorIdx, rst)
        val clusterIdxOpt =
            selector(extraction, rng)
        val newRestaurant =
            insert(clusterIdxOpt, extraction)
        recurseRAC(topicVectorIdx + 1, newRestaurant)
      }
    }
    recurseRAC(0, restaurant)
  }

  @elidable(INFO) def printInfo(msg: String): Unit = println(msg)

  def findClusters(
    maxIterations: Int,
    rng: RandomNumGen,
    selector: (Extraction, RandomNumGen) => Option[Int],
    shuffle: Boolean = true
  ): CRPResult = {
    @tailrec
    def recurseFC(iterationCount: Int, restaurant: Restaurant, bestRestaurant: Restaurant, bestScore: Double): CRPResult = {
      // printInfo(s"iteration $iterationCount: cluster count was ${restaurant.clusters.size} --> ${restaurant.clusters.map(cluster => cluster.topicVectors.size)}")
      if (iterationCount == maxIterations) {

        val clusterResults = bestRestaurant.clusters.zipWithIndex.map{case (c, clusterIndex) =>

          def topicVectorResult(tv: TopicVector): TopicVectorResult = {

            val unnormalizedWeights: ParVector[Double] = parClusterWeights(tv, bestRestaurant.clusters)
            val maxWeight: Double = unnormalizedWeights.max
            val unloggedMaxRemovedWeights: ParVector[Double] = unnormalizedWeights.map(uw => Math.exp(uw - maxWeight))
            val sum: Double = unloggedMaxRemovedWeights.sum
            val belongProb = unloggedMaxRemovedWeights(clusterIndex) / sum

            TopicVectorResult(tv.id, tv.vecMap.toMap, belongProb)
          }

          ClusterResult(
            params,
            c.topicVectors.map(topicVectorResult),
            c.topicCountsSums.toMap
          )
        }
        CRPResult(iterationCount, clusterResults)
      }
      else {
        val (newRestaurant, millis) = time { reseatAllCustomers(restaurant, rng, selector) }
        // printInfo(s" --> ${restaurant.clusters.map(cluster => cluster.topicVectors.size)}")
        val newScore = newRestaurant.logPosteriorProbability
        val (newBestRestaurant, newBestScore) =
          if (newScore > bestScore) {
            // println("And we have a new winner!")
            (newRestaurant, newScore)
          }
          else (bestRestaurant, bestScore)
        // println("logPosteriorProbability was " + newScore)
        val asterisk: String = if (newScore > bestScore) "*" else ""
        printInfo(f"Iteration ${iterationCount + 1}: cluster count was ${restaurant.clusters.size}, reseat: $millis, score: ${newScore}%.5f${asterisk}")
        recurseFC(iterationCount + 1, newRestaurant, newBestRestaurant, newBestScore)
      }
    }
    val initClusters = initializeClusters(shuffle)
    val initRestaurant = Restaurant(initClusters)
    val initScore = initRestaurant.logPosteriorProbability
    recurseFC(0, initRestaurant, initRestaurant, initScore)
  }

  def selectCluster(
    extraction: Extraction,
    rng: RandomNumGen
  ): Option[Int] = {
    val (choice, probsLen): (Int, Int) =
      if (extraction.restaurant.clusters.length > 1000) {
        val probs: Vector[Double] = parTableAssignmentProbabilities(extraction.extractedIdx, extraction.extracted, extraction.restaurant.clusters)
        (weightedProbabilitySamplingForPar(probs, rng), probs.length)
      }
      else {
        val probs: Array[Double] = tableAssignmentProbabilities(extraction.extractedIdx, extraction.extracted, extraction.restaurant.clusters)
        (weightedProbabilitySampling(probs, rng), probs.length)
      }
    if (choice == probsLen - 1) None else Some(choice)
  }
}
