package com.monsanto.stats.tables.clustering

import breeze.linalg.{Vector => _, _}
import breeze.stats.distributions._
import scala.util.Random
import scala.collection.mutable

object MnMGen {

  def getData(): Vector[TopicVectorInput] = {

    val sm = 0.0001

    val xmas = DenseVector(Array(sm, .5 - (sm * 3.0/2.0), sm, .5 - (sm * 3.0/2.0), sm))
    val jul4 = DenseVector(Array((1.0/3.0) - (sm * 2.0/3.0), sm, sm, (1.0 / 3.0) - (sm * 2.0/3.0),  (1.0 / 3.0) - (sm * 2.0/3.0)))
    val regu = DenseVector(Array.fill(5)(1.0 / 5.0))

    val xmasM = Multinomial(xmas)
    val reguM = Multinomial(regu)
    val jul4M = Multinomial(jul4)

    def gimmeABag(id: Long, m: Multinomial[DenseVector[Double],Int]): TopicVectorInput = {
       val indexes = Vector.fill(50)(m.draw())
       val hist = indexes.groupBy(i => i).mapValues(_.length)
       val mutaMap = mutable.Map.empty[Int, Int]
       hist foreach { case (idx, count) => mutaMap(idx) = count }
       TopicVectorInput(id, mutaMap.toMap)
    }
/*
    def gimmeABag(id: Long, m: Multinomial[DenseVector[Double],Int]): (Long, Vector[Int]) = {
       val indexes = Vector.fill(50)(m.draw())
       val hist = indexes.groupBy(i => i).mapValues(_.length)
       val arr = Array(0, 0, 0, 0, 0)
       hist foreach { case (idx, count) => arr(idx) = count }
       (id, arr.toVector)
    }
*/

    /*
     * XMas 14
     * Jul4 3
     * Regu 365 - 17 = 348
     *
     * scala> "xmas".map(_.toInt).sum
     * res2: Int = 441
     *
     * scala> "jul4".map(_.toInt).sum
     * res3: Int = 383
     * 
     * scala> "regu".map(_.toInt).sum
     * res4: Int = 435
     */
    val xmasId = 441L * 10000
    val jul4Id = 383L * 10000
    val reguId = 435L * 10000

    val xmasBags = (for (i <- 0 until 14) yield gimmeABag(xmasId + i, xmasM)).toVector
    val reguBags = (for (i <- 0 until 348) yield gimmeABag(reguId + i, reguM)).toVector
    val jul4Bags = (for (i <- 0 until 3) yield gimmeABag(jul4Id + i, jul4M)).toVector
 
    val samples = xmasBags ++ reguBags ++ jul4Bags
 
    Random.shuffle(samples)
  }

  def main(args: Array[String]): Unit = {

    val dataSet: Vector[TopicVectorInput] = getData()

    println("Name,Blue,Green,Orange,Red,White")

    dataSet foreach { case TopicVectorInput(name, leMap) =>
      var arr = Array(0, 0, 0, 0, 0)
      for ((idx, cnt) <- leMap.toVector) arr(idx.toInt) = cnt
      // println(s"GOT: ${arr.toVector}")
      val Vector(b, g, o, r, w) = arr.toVector
      println(s"$name,$b,$g,$o,$r,$w")
    }
  }

  /*
    A vector of topic vectors, victor, that getData returned one fine morning. Since
    getData likes to mix it up, this method can be used during performance testing
    to get getData data that is always the same.
  */
  def cannedData: Vector[TopicVectorInput] = {
    Vector(
      TopicVectorInput(4350272,Map(0 -> 15, 1 -> 9, 2 -> 9, 3 -> 9, 4 -> 8)),
      TopicVectorInput(4350331,Map(0 -> 10, 1 -> 11, 2 -> 8, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350313,Map(0 -> 10, 1 -> 10, 2 -> 13, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350314,Map(0 -> 17, 1 -> 6, 2 -> 14, 3 -> 10, 4 -> 3)),
      TopicVectorInput(4350145,Map(0 -> 7, 1 -> 11, 2 -> 17, 3 -> 7, 4 -> 8)),
      TopicVectorInput(4350256,Map(0 -> 12, 1 -> 7, 2 -> 14, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350181,Map(0 -> 9, 1 -> 12, 2 -> 10, 3 -> 14, 4 -> 5)),
      TopicVectorInput(4350132,Map(0 -> 13, 1 -> 11, 2 -> 11, 3 -> 6, 4 -> 9)),
      TopicVectorInput(4350002,Map(0 -> 12, 1 -> 5, 2 -> 12, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350133,Map(0 -> 5, 1 -> 16, 2 -> 11, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350114,Map(0 -> 8, 1 -> 16, 2 -> 6, 3 -> 12, 4 -> 8)),
      TopicVectorInput(3830001,Map(4 -> 19, 3 -> 15, 0 -> 16)),
      TopicVectorInput(4350252,Map(0 -> 7, 1 -> 9, 2 -> 9, 3 -> 10, 4 -> 15)),
      TopicVectorInput(4350324,Map(0 -> 15, 1 -> 9, 2 -> 8, 3 -> 11, 4 -> 7)),
      TopicVectorInput(4350208,Map(0 -> 10, 1 -> 11, 2 -> 14, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4350335,Map(0 -> 6, 1 -> 8, 2 -> 12, 3 -> 12, 4 -> 12)),
      TopicVectorInput(4350340,Map(0 -> 9, 1 -> 11, 2 -> 12, 3 -> 11, 4 -> 7)),
      TopicVectorInput(4350171,Map(0 -> 11, 1 -> 12, 2 -> 10, 3 -> 7, 4 -> 10)),
      TopicVectorInput(4350122,Map(0 -> 9, 1 -> 11, 2 -> 8, 3 -> 10, 4 -> 12)),
      TopicVectorInput(4350289,Map(0 -> 8, 1 -> 12, 2 -> 8, 3 -> 10, 4 -> 12)),
      TopicVectorInput(4350243,Map(0 -> 12, 1 -> 5, 2 -> 15, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350312,Map(0 -> 12, 1 -> 15, 2 -> 5, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350337,Map(0 -> 11, 1 -> 9, 2 -> 8, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350047,Map(0 -> 10, 1 -> 10, 2 -> 9, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350260,Map(0 -> 11, 1 -> 6, 2 -> 12, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350298,Map(0 -> 7, 1 -> 15, 2 -> 9, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350042,Map(0 -> 9, 1 -> 11, 2 -> 12, 3 -> 11, 4 -> 7)),
      TopicVectorInput(4350205,Map(0 -> 6, 1 -> 14, 2 -> 12, 3 -> 7, 4 -> 11)),
      TopicVectorInput(4350045,Map(0 -> 12, 1 -> 10, 2 -> 10, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350065,Map(0 -> 9, 1 -> 14, 2 -> 8, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350098,Map(0 -> 10, 1 -> 14, 2 -> 10, 3 -> 6, 4 -> 10)),
      TopicVectorInput(4350183,Map(0 -> 9, 1 -> 7, 2 -> 6, 3 -> 15, 4 -> 13)),
      TopicVectorInput(4350204,Map(0 -> 13, 1 -> 5, 2 -> 13, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350064,Map(0 -> 16, 1 -> 7, 2 -> 9, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350041,Map(0 -> 13, 1 -> 10, 2 -> 8, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350023,Map(0 -> 9, 1 -> 8, 2 -> 8, 3 -> 14, 4 -> 11)),
      TopicVectorInput(4350215,Map(0 -> 12, 1 -> 10, 2 -> 8, 3 -> 7, 4 -> 13)),
      TopicVectorInput(4350009,Map(0 -> 9, 1 -> 7, 2 -> 11, 3 -> 15, 4 -> 8)),
      TopicVectorInput(4350294,Map(0 -> 7, 1 -> 8, 2 -> 15, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350209,Map(0 -> 12, 1 -> 8, 2 -> 6, 3 -> 12, 4 -> 12)),
      TopicVectorInput(4350090,Map(0 -> 13, 1 -> 8, 2 -> 11, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350232,Map(0 -> 8, 1 -> 6, 2 -> 11, 3 -> 9, 4 -> 16)),
      TopicVectorInput(4350185,Map(0 -> 8, 1 -> 11, 2 -> 10, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350019,Map(0 -> 9, 1 -> 6, 2 -> 14, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350213,Map(0 -> 6, 1 -> 10, 2 -> 12, 3 -> 11, 4 -> 11)),
      TopicVectorInput(4350063,Map(0 -> 13, 1 -> 11, 2 -> 11, 3 -> 7, 4 -> 8)),
      TopicVectorInput(4350218,Map(0 -> 9, 1 -> 13, 2 -> 10, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350199,Map(0 -> 15, 1 -> 9, 2 -> 10, 3 -> 7, 4 -> 9)),
      TopicVectorInput(4350227,Map(0 -> 10, 1 -> 12, 2 -> 8, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4350111,Map(0 -> 15, 1 -> 10, 2 -> 3, 3 -> 11, 4 -> 11)),
      TopicVectorInput(4350310,Map(0 -> 16, 1 -> 6, 2 -> 8, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350092,Map(0 -> 11, 1 -> 9, 2 -> 9, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350105,Map(0 -> 14, 1 -> 6, 2 -> 6, 3 -> 11, 4 -> 13)),
      TopicVectorInput(4350201,Map(0 -> 13, 1 -> 7, 2 -> 10, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350022,Map(0 -> 10, 1 -> 15, 2 -> 7, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350073,Map(0 -> 7, 1 -> 10, 2 -> 10, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350175,Map(0 -> 13, 1 -> 16, 2 -> 6, 3 -> 8, 4 -> 7)),
      TopicVectorInput(4350257,Map(0 -> 8, 1 -> 15, 2 -> 8, 3 -> 12, 4 -> 7)),
      TopicVectorInput(4350347,Map(0 -> 10, 1 -> 9, 2 -> 12, 3 -> 13, 4 -> 6)),
      TopicVectorInput(4350277,Map(0 -> 10, 1 -> 15, 2 -> 8, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350286,Map(0 -> 15, 1 -> 7, 2 -> 8, 3 -> 13, 4 -> 7)),
      TopicVectorInput(4350210,Map(0 -> 9, 1 -> 10, 2 -> 11, 3 -> 7, 4 -> 13)),
      TopicVectorInput(4350279,Map(0 -> 7, 1 -> 9, 2 -> 13, 3 -> 10, 4 -> 11)),
      TopicVectorInput(4350068,Map(0 -> 9, 1 -> 8, 2 -> 11, 3 -> 17, 4 -> 5)),
      TopicVectorInput(4350103,Map(0 -> 15, 1 -> 9, 2 -> 8, 3 -> 7, 4 -> 11)),
      TopicVectorInput(4350074,Map(0 -> 3, 1 -> 7, 2 -> 13, 3 -> 9, 4 -> 18)),
      TopicVectorInput(4350038,Map(0 -> 9, 1 -> 9, 2 -> 11, 3 -> 9, 4 -> 12)),
      TopicVectorInput(4350266,Map(0 -> 9, 1 -> 10, 2 -> 14, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350143,Map(0 -> 5, 1 -> 16, 2 -> 11, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4410012,Map(1 -> 19, 3 -> 31)),
      TopicVectorInput(4350057,Map(0 -> 11, 1 -> 13, 2 -> 10, 3 -> 12, 4 -> 4)),
      TopicVectorInput(4350334,Map(0 -> 8, 1 -> 14, 2 -> 12, 3 -> 10, 4 -> 6)),
      TopicVectorInput(4350150,Map(0 -> 13, 1 -> 6, 2 -> 12, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350108,Map(0 -> 12, 1 -> 7, 2 -> 13, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350075,Map(0 -> 12, 1 -> 7, 2 -> 11, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350006,Map(0 -> 11, 1 -> 8, 2 -> 12, 3 -> 12, 4 -> 7)),
      TopicVectorInput(4350127,Map(0 -> 8, 1 -> 20, 2 -> 8, 3 -> 7, 4 -> 7)),
      TopicVectorInput(4410010,Map(1 -> 23, 3 -> 27)),
      TopicVectorInput(4350027,Map(0 -> 10, 1 -> 3, 2 -> 18, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350267,Map(0 -> 11, 1 -> 8, 2 -> 6, 3 -> 10, 4 -> 15)),
      TopicVectorInput(4350322,Map(0 -> 10, 1 -> 12, 2 -> 9, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4410013,Map(1 -> 19, 3 -> 31)),
      TopicVectorInput(4350194,Map(0 -> 13, 1 -> 12, 2 -> 8, 3 -> 9, 4 -> 8)),
      TopicVectorInput(4350091,Map(0 -> 9, 1 -> 11, 2 -> 12, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350152,Map(0 -> 13, 1 -> 8, 2 -> 6, 3 -> 10, 4 -> 13)),
      TopicVectorInput(4350158,Map(0 -> 9, 1 -> 12, 2 -> 12, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350281,Map(0 -> 7, 1 -> 5, 2 -> 15, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350106,Map(0 -> 9, 1 -> 8, 2 -> 11, 3 -> 16, 4 -> 6)),
      TopicVectorInput(4350318,Map(0 -> 13, 1 -> 10, 2 -> 14, 3 -> 6, 4 -> 7)),
      TopicVectorInput(4350048,Map(0 -> 4, 1 -> 11, 2 -> 9, 3 -> 12, 4 -> 14)),
      TopicVectorInput(4350044,Map(0 -> 7, 1 -> 8, 2 -> 14, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350287,Map(0 -> 17, 1 -> 7, 2 -> 9, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350082,Map(0 -> 8, 1 -> 12, 2 -> 11, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4410005,Map(1 -> 29, 3 -> 21)),
      TopicVectorInput(4350032,Map(0 -> 7, 1 -> 9, 2 -> 7, 3 -> 9, 4 -> 18)),
      TopicVectorInput(4350033,Map(0 -> 15, 1 -> 9, 2 -> 9, 3 -> 9, 4 -> 8)),
      TopicVectorInput(4350077,Map(0 -> 12, 1 -> 9, 2 -> 11, 3 -> 5, 4 -> 13)),
      TopicVectorInput(4350134,Map(0 -> 10, 1 -> 7, 2 -> 13, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350332,Map(0 -> 13, 1 -> 7, 2 -> 9, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350237,Map(0 -> 8, 1 -> 11, 2 -> 13, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350188,Map(0 -> 13, 1 -> 6, 2 -> 8, 3 -> 12, 4 -> 11)),
      TopicVectorInput(4350160,Map(0 -> 13, 1 -> 8, 2 -> 12, 3 -> 6, 4 -> 11)),
      TopicVectorInput(4350263,Map(0 -> 9, 1 -> 17, 2 -> 9, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4350174,Map(0 -> 11, 1 -> 10, 2 -> 7, 3 -> 10, 4 -> 12)),
      TopicVectorInput(4350325,Map(0 -> 11, 1 -> 8, 2 -> 8, 3 -> 13, 4 -> 10)),
      TopicVectorInput(4350149,Map(0 -> 11, 1 -> 9, 2 -> 8, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350015,Map(0 -> 14, 1 -> 13, 2 -> 7, 3 -> 7, 4 -> 9)),
      TopicVectorInput(4350338,Map(0 -> 13, 1 -> 7, 2 -> 9, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350343,Map(0 -> 8, 1 -> 13, 2 -> 13, 3 -> 10, 4 -> 6)),
      TopicVectorInput(4350067,Map(0 -> 9, 1 -> 10, 2 -> 11, 3 -> 11, 4 -> 9)),
      TopicVectorInput(4350102,Map(0 -> 9, 1 -> 12, 2 -> 6, 3 -> 9, 4 -> 14)),
      TopicVectorInput(4350100,Map(0 -> 15, 1 -> 7, 2 -> 7, 3 -> 9, 4 -> 12)),
      TopicVectorInput(4350116,Map(0 -> 9, 1 -> 12, 2 -> 10, 3 -> 5, 4 -> 14)),
      TopicVectorInput(4350197,Map(0 -> 13, 1 -> 7, 2 -> 6, 3 -> 11, 4 -> 13)),
      TopicVectorInput(4350168,Map(0 -> 20, 1 -> 8, 2 -> 4, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350039,Map(0 -> 7, 1 -> 12, 2 -> 10, 3 -> 9, 4 -> 12)),
      TopicVectorInput(4350198,Map(0 -> 6, 1 -> 15, 2 -> 7, 3 -> 16, 4 -> 6)),
      TopicVectorInput(4350292,Map(0 -> 7, 1 -> 10, 2 -> 12, 3 -> 4, 4 -> 17)),
      TopicVectorInput(4350094,Map(0 -> 7, 1 -> 14, 2 -> 9, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350170,Map(0 -> 14, 1 -> 9, 2 -> 8, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350112,Map(0 -> 8, 1 -> 13, 2 -> 11, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350129,Map(0 -> 9, 1 -> 13, 2 -> 9, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350034,Map(0 -> 11, 1 -> 8, 2 -> 7, 3 -> 12, 4 -> 12)),
      TopicVectorInput(4350290,Map(0 -> 13, 1 -> 8, 2 -> 13, 3 -> 12, 4 -> 4)),
      TopicVectorInput(4350119,Map(0 -> 7, 1 -> 9, 2 -> 11, 3 -> 8, 4 -> 15)),
      TopicVectorInput(4350295,Map(0 -> 10, 1 -> 8, 2 -> 13, 3 -> 7, 4 -> 12)),
      TopicVectorInput(4350216,Map(0 -> 11, 1 -> 13, 2 -> 3, 3 -> 9, 4 -> 14)),
      TopicVectorInput(4350104,Map(0 -> 9, 1 -> 12, 2 -> 8, 3 -> 10, 4 -> 11)),
      TopicVectorInput(4350004,Map(0 -> 9, 1 -> 10, 2 -> 10, 3 -> 14, 4 -> 7)),
      TopicVectorInput(4350269,Map(0 -> 10, 1 -> 7, 2 -> 9, 3 -> 14, 4 -> 10)),
      TopicVectorInput(4350187,Map(0 -> 7, 1 -> 12, 2 -> 9, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350273,Map(0 -> 13, 1 -> 8, 2 -> 15, 3 -> 10, 4 -> 4)),
      TopicVectorInput(4350303,Map(0 -> 12, 1 -> 11, 2 -> 9, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350086,Map(0 -> 15, 1 -> 6, 2 -> 12, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350159,Map(0 -> 14, 1 -> 12, 2 -> 8, 3 -> 10, 4 -> 6)),
      TopicVectorInput(4350320,Map(0 -> 6, 1 -> 8, 2 -> 13, 3 -> 13, 4 -> 10)),
      TopicVectorInput(4350141,Map(0 -> 15, 1 -> 14, 2 -> 4, 3 -> 6, 4 -> 11)),
      TopicVectorInput(4350124,Map(0 -> 10, 1 -> 9, 2 -> 8, 3 -> 12, 4 -> 11)),
      TopicVectorInput(4350096,Map(0 -> 13, 1 -> 11, 2 -> 13, 3 -> 8, 4 -> 5)),
      TopicVectorInput(4350250,Map(0 -> 10, 1 -> 13, 2 -> 7, 3 -> 13, 4 -> 7)),
      TopicVectorInput(4350223,Map(0 -> 17, 1 -> 9, 2 -> 9, 3 -> 10, 4 -> 5)),
      TopicVectorInput(4350025,Map(0 -> 9, 1 -> 10, 2 -> 10, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350005,Map(0 -> 11, 1 -> 9, 2 -> 9, 3 -> 17, 4 -> 4)),
      TopicVectorInput(4350212,Map(0 -> 7, 1 -> 8, 2 -> 16, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350305,Map(0 -> 13, 1 -> 10, 2 -> 6, 3 -> 7, 4 -> 14)),
      TopicVectorInput(4350309,Map(0 -> 12, 1 -> 8, 2 -> 7, 3 -> 7, 4 -> 16)),
      TopicVectorInput(4350011,Map(0 -> 13, 1 -> 6, 2 -> 9, 3 -> 10, 4 -> 12)),
      TopicVectorInput(4350079,Map(0 -> 10, 1 -> 13, 2 -> 14, 3 -> 6, 4 -> 7)),
      TopicVectorInput(4410004,Map(1 -> 22, 3 -> 28)),
      TopicVectorInput(4350255,Map(0 -> 9, 1 -> 9, 2 -> 11, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350234,Map(0 -> 16, 1 -> 6, 2 -> 9, 3 -> 14, 4 -> 5)),
      TopicVectorInput(4350200,Map(0 -> 8, 1 -> 15, 2 -> 11, 3 -> 5, 4 -> 11)),
      TopicVectorInput(4350327,Map(0 -> 13, 1 -> 8, 2 -> 7, 3 -> 13, 4 -> 9)),
      TopicVectorInput(4350013,Map(0 -> 10, 1 -> 12, 2 -> 6, 3 -> 11, 4 -> 11)),
      TopicVectorInput(4350062,Map(0 -> 10, 1 -> 8, 2 -> 12, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350230,Map(0 -> 11, 1 -> 5, 2 -> 11, 3 -> 18, 4 -> 5)),
      TopicVectorInput(4350214,Map(0 -> 10, 1 -> 12, 2 -> 10, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350148,Map(0 -> 13, 1 -> 7, 2 -> 10, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350297,Map(0 -> 13, 1 -> 8, 2 -> 10, 3 -> 13, 4 -> 6)),
      TopicVectorInput(4350144,Map(0 -> 13, 1 -> 6, 2 -> 10, 3 -> 10, 4 -> 11)),
      TopicVectorInput(4350066,Map(0 -> 10, 1 -> 8, 2 -> 11, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350028,Map(0 -> 11, 1 -> 12, 2 -> 8, 3 -> 11, 4 -> 8)),
      TopicVectorInput(3830000,Map(4 -> 11, 3 -> 16, 0 -> 23)),
      TopicVectorInput(4350275,Map(0 -> 13, 1 -> 7, 2 -> 10, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350346,Map(0 -> 8, 1 -> 7, 2 -> 14, 3 -> 12, 4 -> 9)),
      TopicVectorInput(4350301,Map(0 -> 11, 1 -> 13, 2 -> 7, 3 -> 7, 4 -> 12)),
      TopicVectorInput(4350328,Map(0 -> 8, 1 -> 11, 2 -> 15, 3 -> 8, 4 -> 8)),
      TopicVectorInput(4350231,Map(0 -> 14, 1 -> 7, 2 -> 8, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350043,Map(0 -> 9, 1 -> 10, 2 -> 11, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350193,Map(0 -> 9, 1 -> 13, 2 -> 5, 3 -> 10, 4 -> 13)),
      TopicVectorInput(4350109,Map(0 -> 12, 1 -> 9, 2 -> 17, 3 -> 8, 4 -> 4)),
      TopicVectorInput(4350288,Map(0 -> 13, 1 -> 9, 2 -> 5, 3 -> 15, 4 -> 8)),
      TopicVectorInput(4350076,Map(0 -> 8, 1 -> 9, 2 -> 8, 3 -> 9, 4 -> 16)),
      TopicVectorInput(4350072,Map(0 -> 9, 1 -> 11, 2 -> 12, 3 -> 7, 4 -> 11)),
      TopicVectorInput(4350326,Map(0 -> 8, 1 -> 10, 2 -> 11, 3 -> 14, 4 -> 7)),
      TopicVectorInput(4350156,Map(0 -> 14, 1 -> 10, 2 -> 9, 3 -> 12, 4 -> 5)),
      TopicVectorInput(4350037,Map(0 -> 7, 1 -> 16, 2 -> 10, 3 -> 13, 4 -> 4)),
      TopicVectorInput(4350182,Map(0 -> 8, 1 -> 12, 2 -> 10, 3 -> 13, 4 -> 7)),
      TopicVectorInput(4350296,Map(0 -> 12, 1 -> 12, 2 -> 7, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350248,Map(0 -> 12, 1 -> 13, 2 -> 10, 3 -> 7, 4 -> 8)),
      TopicVectorInput(4350081,Map(0 -> 7, 1 -> 13, 2 -> 8, 3 -> 9, 4 -> 13)),
      TopicVectorInput(4350140,Map(0 -> 9, 1 -> 13, 2 -> 5, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350061,Map(0 -> 11, 1 -> 13, 2 -> 6, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4410009,Map(1 -> 22, 3 -> 28)),
      TopicVectorInput(4350221,Map(0 -> 15, 1 -> 8, 2 -> 5, 3 -> 12, 4 -> 10)),
      TopicVectorInput(4350001,Map(0 -> 12, 1 -> 11, 2 -> 9, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350307,Map(0 -> 10, 1 -> 4, 2 -> 11, 3 -> 11, 4 -> 14)),
      TopicVectorInput(4350030,Map(0 -> 14, 1 -> 15, 2 -> 6, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4350052,Map(0 -> 12, 1 -> 6, 2 -> 12, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350036,Map(0 -> 11, 1 -> 9, 2 -> 11, 3 -> 7, 4 -> 12)),
      TopicVectorInput(4350228,Map(0 -> 13, 1 -> 9, 2 -> 7, 3 -> 14, 4 -> 7)),
      TopicVectorInput(4350135,Map(0 -> 9, 1 -> 6, 2 -> 12, 3 -> 12, 4 -> 11)),
      TopicVectorInput(4350071,Map(0 -> 5, 1 -> 9, 2 -> 15, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350240,Map(0 -> 8, 1 -> 8, 2 -> 9, 3 -> 15, 4 -> 10)),
      TopicVectorInput(4350262,Map(0 -> 8, 1 -> 11, 2 -> 12, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350088,Map(0 -> 10, 1 -> 10, 2 -> 10, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4350154,Map(0 -> 13, 1 -> 16, 2 -> 5, 3 -> 5, 4 -> 11)),
      TopicVectorInput(4350078,Map(0 -> 8, 1 -> 13, 2 -> 18, 3 -> 8, 4 -> 3)),
      TopicVectorInput(4350165,Map(0 -> 9, 1 -> 16, 2 -> 8, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350161,Map(0 -> 13, 1 -> 9, 2 -> 9, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350167,Map(0 -> 9, 1 -> 8, 2 -> 15, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350251,Map(0 -> 9, 1 -> 5, 2 -> 15, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350196,Map(0 -> 8, 1 -> 12, 2 -> 16, 3 -> 8, 4 -> 6)),
      TopicVectorInput(4350253,Map(0 -> 8, 1 -> 8, 2 -> 9, 3 -> 9, 4 -> 16)),
      TopicVectorInput(4350117,Map(0 -> 11, 1 -> 10, 2 -> 10, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350270,Map(0 -> 7, 1 -> 10, 2 -> 13, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4350054,Map(0 -> 13, 1 -> 7, 2 -> 9, 3 -> 10, 4 -> 11)),
      TopicVectorInput(4350179,Map(0 -> 12, 1 -> 9, 2 -> 9, 3 -> 14, 4 -> 6)),
      TopicVectorInput(4350254,Map(0 -> 15, 1 -> 14, 2 -> 8, 3 -> 4, 4 -> 9)),
      TopicVectorInput(4350097,Map(0 -> 7, 1 -> 10, 2 -> 12, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350202,Map(0 -> 13, 1 -> 11, 2 -> 11, 3 -> 12, 4 -> 3)),
      TopicVectorInput(4350244,Map(0 -> 12, 1 -> 11, 2 -> 10, 3 -> 6, 4 -> 11)),
      TopicVectorInput(4350189,Map(0 -> 9, 1 -> 8, 2 -> 18, 3 -> 7, 4 -> 8)),
      TopicVectorInput(4350236,Map(0 -> 11, 1 -> 12, 2 -> 8, 3 -> 7, 4 -> 12)),
      TopicVectorInput(4350060,Map(0 -> 15, 1 -> 3, 2 -> 14, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350180,Map(0 -> 10, 1 -> 5, 2 -> 13, 3 -> 11, 4 -> 11)),
      TopicVectorInput(4350110,Map(0 -> 16, 1 -> 9, 2 -> 11, 3 -> 5, 4 -> 9)),
      TopicVectorInput(4350010,Map(0 -> 9, 1 -> 7, 2 -> 12, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350222,Map(0 -> 8, 1 -> 10, 2 -> 8, 3 -> 11, 4 -> 13)),
      TopicVectorInput(4350246,Map(0 -> 7, 1 -> 9, 2 -> 14, 3 -> 13, 4 -> 7)),
      TopicVectorInput(4350018,Map(0 -> 10, 1 -> 15, 2 -> 5, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350084,Map(0 -> 12, 1 -> 11, 2 -> 4, 3 -> 7, 4 -> 16)),
      TopicVectorInput(4350139,Map(0 -> 13, 1 -> 6, 2 -> 10, 3 -> 7, 4 -> 14)),
      TopicVectorInput(4350241,Map(0 -> 18, 1 -> 7, 2 -> 10, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4350339,Map(0 -> 11, 1 -> 9, 2 -> 9, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350153,Map(0 -> 7, 1 -> 12, 2 -> 10, 3 -> 9, 4 -> 12)),
      TopicVectorInput(4350099,Map(0 -> 14, 1 -> 7, 2 -> 7, 3 -> 13, 4 -> 9)),
      TopicVectorInput(4350040,Map(0 -> 13, 1 -> 7, 2 -> 12, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350203,Map(0 -> 11, 1 -> 10, 2 -> 11, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350184,Map(0 -> 11, 1 -> 13, 2 -> 8, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350190,Map(0 -> 10, 1 -> 14, 2 -> 11, 3 -> 8, 4 -> 7)),
      TopicVectorInput(4350191,Map(0 -> 11, 1 -> 8, 2 -> 10, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350344,Map(0 -> 9, 1 -> 11, 2 -> 5, 3 -> 12, 4 -> 13)),
      TopicVectorInput(4410002,Map(1 -> 28, 3 -> 22)),
      TopicVectorInput(4350014,Map(0 -> 10, 1 -> 11, 2 -> 14, 3 -> 6, 4 -> 9)),
      TopicVectorInput(4350211,Map(0 -> 9, 1 -> 9, 2 -> 13, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4410011,Map(1 -> 23, 3 -> 27)),
      TopicVectorInput(4350276,Map(0 -> 13, 1 -> 10, 2 -> 8, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350058,Map(0 -> 15, 1 -> 15, 2 -> 5, 3 -> 8, 4 -> 7)),
      TopicVectorInput(4350245,Map(0 -> 7, 1 -> 10, 2 -> 13, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350280,Map(0 -> 9, 1 -> 15, 2 -> 13, 3 -> 6, 4 -> 7)),
      TopicVectorInput(4350345,Map(0 -> 8, 1 -> 7, 2 -> 7, 3 -> 14, 4 -> 14)),
      TopicVectorInput(4350051,Map(0 -> 11, 1 -> 9, 2 -> 8, 3 -> 12, 4 -> 10)),
      TopicVectorInput(4350085,Map(0 -> 13, 1 -> 13, 2 -> 5, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350249,Map(0 -> 11, 1 -> 7, 2 -> 7, 3 -> 12, 4 -> 13)),
      TopicVectorInput(4410000,Map(1 -> 25, 3 -> 25)),
      TopicVectorInput(4350050,Map(0 -> 9, 1 -> 11, 2 -> 10, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4350147,Map(0 -> 7, 1 -> 10, 2 -> 11, 3 -> 13, 4 -> 9)),
      TopicVectorInput(4350206,Map(0 -> 8, 1 -> 10, 2 -> 13, 3 -> 14, 4 -> 5)),
      TopicVectorInput(4350120,Map(0 -> 10, 1 -> 9, 2 -> 11, 3 -> 6, 4 -> 14)),
      TopicVectorInput(4350225,Map(0 -> 7, 1 -> 16, 2 -> 10, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350258,Map(0 -> 10, 1 -> 10, 2 -> 8, 3 -> 15, 4 -> 7)),
      TopicVectorInput(4350017,Map(0 -> 5, 1 -> 9, 2 -> 9, 3 -> 14, 4 -> 13)),
      TopicVectorInput(4350284,Map(0 -> 9, 1 -> 7, 2 -> 11, 3 -> 10, 4 -> 13)),
      TopicVectorInput(4350177,Map(0 -> 10, 1 -> 12, 2 -> 8, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350130,Map(0 -> 7, 1 -> 8, 2 -> 13, 3 -> 6, 4 -> 16)),
      TopicVectorInput(4350070,Map(0 -> 10, 1 -> 12, 2 -> 13, 3 -> 8, 4 -> 7)),
      TopicVectorInput(4350093,Map(0 -> 8, 1 -> 11, 2 -> 10, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350283,Map(0 -> 9, 1 -> 16, 2 -> 8, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350274,Map(0 -> 12, 1 -> 11, 2 -> 9, 3 -> 11, 4 -> 7)),
      TopicVectorInput(4350195,Map(0 -> 9, 1 -> 13, 2 -> 7, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350113,Map(0 -> 8, 1 -> 7, 2 -> 8, 3 -> 15, 4 -> 12)),
      TopicVectorInput(4350123,Map(0 -> 10, 1 -> 9, 2 -> 11, 3 -> 9, 4 -> 11)),
      TopicVectorInput(4350330,Map(0 -> 6, 1 -> 10, 2 -> 11, 3 -> 15, 4 -> 8)),
      TopicVectorInput(4350101,Map(0 -> 4, 1 -> 4, 2 -> 17, 3 -> 17, 4 -> 8)),
      TopicVectorInput(4350192,Map(0 -> 16, 1 -> 7, 2 -> 10, 3 -> 9, 4 -> 8)),
      TopicVectorInput(4350239,Map(0 -> 7, 1 -> 16, 2 -> 10, 3 -> 4, 4 -> 13)),
      TopicVectorInput(4410003,Map(1 -> 19, 3 -> 31)),
      TopicVectorInput(4350151,Map(0 -> 14, 1 -> 8, 2 -> 9, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350049,Map(0 -> 10, 1 -> 4, 2 -> 13, 3 -> 9, 4 -> 14)),
      TopicVectorInput(4350125,Map(0 -> 9, 1 -> 9, 2 -> 10, 3 -> 7, 4 -> 15)),
      TopicVectorInput(4350238,Map(0 -> 12, 1 -> 10, 2 -> 14, 3 -> 6, 4 -> 8)),
      TopicVectorInput(4350080,Map(0 -> 10, 1 -> 7, 2 -> 8, 3 -> 13, 4 -> 12)),
      TopicVectorInput(4350126,Map(0 -> 14, 1 -> 12, 2 -> 8, 3 -> 10, 4 -> 6)),
      TopicVectorInput(4350285,Map(0 -> 9, 1 -> 7, 2 -> 9, 3 -> 12, 4 -> 13)),
      TopicVectorInput(4350178,Map(0 -> 4, 1 -> 11, 2 -> 12, 3 -> 6, 4 -> 17)),
      TopicVectorInput(4350323,Map(0 -> 6, 1 -> 21, 2 -> 8, 3 -> 8, 4 -> 7)),
      TopicVectorInput(4350333,Map(0 -> 14, 1 -> 11, 2 -> 8, 3 -> 10, 4 -> 7)),
      TopicVectorInput(4350261,Map(0 -> 12, 1 -> 10, 2 -> 7, 3 -> 11, 4 -> 10)),
      TopicVectorInput(4350131,Map(0 -> 6, 1 -> 8, 2 -> 15, 3 -> 10, 4 -> 11)),
      TopicVectorInput(4350317,Map(0 -> 8, 1 -> 14, 2 -> 5, 3 -> 12, 4 -> 11)),
      TopicVectorInput(4350046,Map(0 -> 16, 1 -> 7, 2 -> 8, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350293,Map(0 -> 8, 1 -> 13, 2 -> 9, 3 -> 11, 4 -> 9)),
      TopicVectorInput(4350164,Map(0 -> 10, 1 -> 7, 2 -> 7, 3 -> 12, 4 -> 14)),
      TopicVectorInput(4350069,Map(0 -> 10, 1 -> 10, 2 -> 10, 3 -> 8, 4 -> 12)),
      TopicVectorInput(4350026,Map(0 -> 17, 1 -> 9, 2 -> 6, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350162,Map(0 -> 10, 1 -> 8, 2 -> 9, 3 -> 10, 4 -> 13)),
      TopicVectorInput(4350247,Map(0 -> 11, 1 -> 11, 2 -> 6, 3 -> 13, 4 -> 9)),
      TopicVectorInput(4350053,Map(0 -> 5, 1 -> 13, 2 -> 13, 3 -> 13, 4 -> 6)),
      TopicVectorInput(4350059,Map(0 -> 3, 1 -> 6, 2 -> 13, 3 -> 13, 4 -> 15)),
      TopicVectorInput(4350315,Map(0 -> 13, 1 -> 10, 2 -> 10, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350282,Map(0 -> 11, 1 -> 13, 2 -> 10, 3 -> 5, 4 -> 11)),
      TopicVectorInput(4350176,Map(0 -> 11, 1 -> 8, 2 -> 12, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350259,Map(0 -> 10, 1 -> 14, 2 -> 9, 3 -> 9, 4 -> 8)),
      TopicVectorInput(4350329,Map(0 -> 8, 1 -> 13, 2 -> 14, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4350138,Map(0 -> 15, 1 -> 14, 2 -> 9, 3 -> 6, 4 -> 6)),
      TopicVectorInput(4350024,Map(0 -> 10, 1 -> 8, 2 -> 7, 3 -> 12, 4 -> 13)),
      TopicVectorInput(4350128,Map(0 -> 8, 1 -> 10, 2 -> 10, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350016,Map(0 -> 14, 1 -> 5, 2 -> 7, 3 -> 12, 4 -> 12)),
      TopicVectorInput(4350229,Map(0 -> 14, 1 -> 10, 2 -> 8, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350137,Map(0 -> 6, 1 -> 12, 2 -> 11, 3 -> 7, 4 -> 14)),
      TopicVectorInput(4350308,Map(0 -> 8, 1 -> 7, 2 -> 9, 3 -> 14, 4 -> 12)),
      TopicVectorInput(4410007,Map(1 -> 19, 3 -> 31)),
      TopicVectorInput(4350169,Map(0 -> 9, 1 -> 12, 2 -> 10, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350056,Map(0 -> 6, 1 -> 11, 2 -> 13, 3 -> 11, 4 -> 9)),
      TopicVectorInput(4350224,Map(0 -> 9, 1 -> 9, 2 -> 15, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350121,Map(0 -> 9, 1 -> 7, 2 -> 11, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350107,Map(0 -> 7, 1 -> 14, 2 -> 12, 3 -> 6, 4 -> 11)),
      TopicVectorInput(4350271,Map(0 -> 8, 1 -> 6, 2 -> 12, 3 -> 13, 4 -> 11)),
      TopicVectorInput(4410008,Map(1 -> 27, 3 -> 23)),
      TopicVectorInput(4410001,Map(1 -> 23, 3 -> 27)),
      TopicVectorInput(4350265,Map(0 -> 5, 1 -> 12, 2 -> 14, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350155,Map(0 -> 18, 1 -> 7, 2 -> 5, 3 -> 6, 4 -> 14)),
      TopicVectorInput(4350342,Map(0 -> 8, 1 -> 10, 2 -> 7, 3 -> 15, 4 -> 10)),
      TopicVectorInput(4350008,Map(0 -> 7, 1 -> 10, 2 -> 11, 3 -> 14, 4 -> 8)),
      TopicVectorInput(4350336,Map(0 -> 10, 1 -> 13, 2 -> 8, 3 -> 11, 4 -> 8)),
      TopicVectorInput(4350291,Map(0 -> 5, 1 -> 11, 2 -> 13, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350311,Map(0 -> 11, 1 -> 13, 2 -> 7, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350207,Map(0 -> 15, 1 -> 9, 2 -> 10, 3 -> 6, 4 -> 10)),
      TopicVectorInput(4350264,Map(0 -> 6, 1 -> 15, 2 -> 5, 3 -> 12, 4 -> 12)),
      TopicVectorInput(4350217,Map(0 -> 15, 1 -> 10, 2 -> 8, 3 -> 11, 4 -> 6)),
      TopicVectorInput(4350300,Map(0 -> 13, 1 -> 7, 2 -> 11, 3 -> 7, 4 -> 12)),
      TopicVectorInput(4350235,Map(0 -> 5, 1 -> 10, 2 -> 16, 3 -> 8, 4 -> 11)),
      TopicVectorInput(4350278,Map(0 -> 7, 1 -> 11, 2 -> 14, 3 -> 6, 4 -> 12)),
      TopicVectorInput(4350095,Map(0 -> 15, 1 -> 6, 2 -> 11, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350172,Map(0 -> 8, 1 -> 9, 2 -> 10, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350220,Map(0 -> 12, 1 -> 9, 2 -> 9, 3 -> 13, 4 -> 7)),
      TopicVectorInput(4350136,Map(0 -> 10, 1 -> 10, 2 -> 12, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350157,Map(0 -> 10, 1 -> 8, 2 -> 9, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350226,Map(0 -> 11, 1 -> 7, 2 -> 12, 3 -> 10, 4 -> 10)),
      TopicVectorInput(4350321,Map(0 -> 10, 1 -> 10, 2 -> 16, 3 -> 9, 4 -> 5)),
      TopicVectorInput(4350186,Map(0 -> 3, 1 -> 12, 2 -> 17, 3 -> 9, 4 -> 9)),
      TopicVectorInput(4350299,Map(0 -> 9, 1 -> 7, 2 -> 19, 3 -> 9, 4 -> 6)),
      TopicVectorInput(4410006,Map(1 -> 20, 3 -> 30)),
      TopicVectorInput(4350031,Map(0 -> 9, 1 -> 15, 2 -> 10, 3 -> 7, 4 -> 9)),
      TopicVectorInput(4350163,Map(0 -> 9, 1 -> 6, 2 -> 8, 3 -> 13, 4 -> 14)),
      TopicVectorInput(4350319,Map(0 -> 14, 1 -> 9, 2 -> 6, 3 -> 13, 4 -> 8)),
      TopicVectorInput(4350087,Map(0 -> 10, 1 -> 11, 2 -> 9, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350115,Map(0 -> 6, 1 -> 10, 2 -> 14, 3 -> 12, 4 -> 8)),
      TopicVectorInput(4350268,Map(0 -> 10, 1 -> 6, 2 -> 22, 3 -> 9, 4 -> 3)),
      TopicVectorInput(4350083,Map(0 -> 12, 1 -> 13, 2 -> 9, 3 -> 8, 4 -> 8)),
      TopicVectorInput(4350173,Map(0 -> 7, 1 -> 9, 2 -> 10, 3 -> 14, 4 -> 10)),
      TopicVectorInput(4350316,Map(0 -> 6, 1 -> 16, 2 -> 11, 3 -> 5, 4 -> 12)),
      TopicVectorInput(4350000,Map(0 -> 14, 1 -> 5, 2 -> 10, 3 -> 8, 4 -> 13)),
      TopicVectorInput(4350233,Map(0 -> 16, 1 -> 7, 2 -> 8, 3 -> 6, 4 -> 13)),
      TopicVectorInput(4350118,Map(0 -> 7, 1 -> 11, 2 -> 10, 3 -> 6, 4 -> 16)),
      TopicVectorInput(3830002,Map(4 -> 18, 3 -> 10, 0 -> 22)),
      TopicVectorInput(4350021,Map(0 -> 15, 1 -> 10, 2 -> 12, 3 -> 5, 4 -> 8)),
      TopicVectorInput(4350020,Map(0 -> 14, 1 -> 9, 2 -> 6, 3 -> 7, 4 -> 14)),
      TopicVectorInput(4350219,Map(0 -> 11, 1 -> 5, 2 -> 15, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350003,Map(0 -> 11, 1 -> 13, 2 -> 9, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350302,Map(0 -> 14, 1 -> 10, 2 -> 8, 3 -> 10, 4 -> 8)),
      TopicVectorInput(4350035,Map(0 -> 8, 1 -> 10, 2 -> 14, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350166,Map(0 -> 10, 1 -> 11, 2 -> 12, 3 -> 11, 4 -> 6)),
      TopicVectorInput(4350341,Map(0 -> 8, 1 -> 7, 2 -> 11, 3 -> 7, 4 -> 17)),
      TopicVectorInput(4350242,Map(0 -> 11, 1 -> 8, 2 -> 8, 3 -> 11, 4 -> 12)),
      TopicVectorInput(4350089,Map(0 -> 13, 1 -> 9, 2 -> 9, 3 -> 10, 4 -> 9)),
      TopicVectorInput(4350146,Map(0 -> 12, 1 -> 11, 2 -> 8, 3 -> 5, 4 -> 14)),
      TopicVectorInput(4350029,Map(0 -> 13, 1 -> 10, 2 -> 9, 3 -> 8, 4 -> 10)),
      TopicVectorInput(4350012,Map(0 -> 9, 1 -> 13, 2 -> 11, 3 -> 7, 4 -> 10)),
      TopicVectorInput(4350007,Map(0 -> 12, 1 -> 10, 2 -> 11, 3 -> 8, 4 -> 9)),
      TopicVectorInput(4350306,Map(0 -> 8, 1 -> 9, 2 -> 10, 3 -> 9, 4 -> 14)),
      TopicVectorInput(4350142,Map(0 -> 10, 1 -> 11, 2 -> 10, 3 -> 9, 4 -> 10)),
      TopicVectorInput(4350304,Map(0 -> 8, 1 -> 13, 2 -> 7, 3 -> 8, 4 -> 14)),
      TopicVectorInput(4350055,Map(0 -> 9, 1 -> 10, 2 -> 8, 3 -> 9, 4 -> 14))
    )
  }
}
