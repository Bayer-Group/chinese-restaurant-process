package com.monsanto.stats.tables.clustering

import scala.util.Random

object DataGen {

  private def cannedDataFrom(csvFile: String): scala.collection.immutable.Vector[TopicVectorInput] = {
    scala.io.Source.fromFile(csvFile).getLines.filter(!_.isEmpty).map { line =>
      val tokens: List[String] = line.split(", ").toList
      val id = tokens.head.toLong
      val vecMap: Map[Int, Int] = Map.empty ++ tokens.tail.grouped(2).map { slice =>
        (slice(0).toInt, slice(1).toInt)
      }
      TopicVectorInput(id, vecMap)
    }.toVector
  }
  
  def cannedBigData: scala.collection.immutable.Vector[TopicVectorInput] = cannedDataFrom("canned-data/big-data.csv")

  // Parens because not functional, has the side affect of generating random numbers
  def getData(): scala.collection.immutable.Vector[TopicVectorInput] = getDataWithRnd(new Random)

  def getDataWithRnd(rnd: Random): scala.collection.immutable.Vector[TopicVectorInput] = {
    import breeze.linalg._
    import breeze.stats.distributions._

    val numberOfBags = 100000
    val vocabularySize = 10000
    val tablesCount = 10

    val minTableSize = 100
    def halves(itemsRemaining: Int, acc: List[Int]): List[Int] = {
      val newBinSize = itemsRemaining / 2
      if(newBinSize <= minTableSize){
        itemsRemaining :: acc
      }
      else {
        halves(itemsRemaining - newBinSize, newBinSize :: acc)
      }
    }
    val tablesSizes = halves(numberOfBags, Nil)

    val sm = 0.0001

    val countOfInterestsDist = new Exponential(1/10.0)
    val topicInterestLevelDist = new Exponential(1/100.0)
    def gimmieInterests(): DenseVector[Double] = { // Returns interests for one table, length 10,000
    val countOfInterests = (countOfInterestsDist.draw() + 1).toInt
      val interestProbs = Array.fill(vocabularySize)(sm) // size 10,000 array, filled initially with .0001
      (0 to countOfInterests).foreach{ _ => // countOfInterests is exponentially distributed
        interestProbs(rnd.nextInt(vocabularySize)) = topicInterestLevelDist.draw() + 10
      }
      val normalizingConstant = interestProbs.sum.toDouble
      DenseVector(interestProbs.map( _ / normalizingConstant)) // now they sum to 1
    }

    val tableTopicsDistributions = Array.fill(tablesCount)(Multinomial(gimmieInterests())) // same as xmasM, reguM...

    val instancePerPersonDist = Gaussian(400, 100)
    def gimmieAPerson(tableIndex: Long, m: Multinomial[DenseVector[Double],Int]): TopicVectorInput = { // like a bag
      val instanceCount = Math.abs(instancePerPersonDist.draw()).toInt
      val instanceTopicIndexes: Map[Int, Int] = Array.fill(instanceCount)(m.draw()).groupBy(i => i).mapValues(_.length)
      TopicVectorInput(tableIndex, instanceTopicIndexes)
    }

    var i = 0L
    val topicVectors: scala.collection.immutable.Vector[TopicVectorInput] = tablesSizes.zipWithIndex.flatMap { case (tableSize, tableIdx) =>
      Array.fill(tableSize){
        gimmieAPerson(tableIdx, tableTopicsDistributions(tableIdx))
      }
    }.map { tv =>
      i += 1
      tv.copy(id = i)
    }.toVector
    println("###topicVectors.map(_.id).distinct.length: " + topicVectors.map(_.id).distinct.length)
    assert(topicVectors.map(_.id).distinct.length == 100000)
    topicVectors
  }
}
  
