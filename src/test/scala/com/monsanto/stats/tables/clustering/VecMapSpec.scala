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

class VecMapSpec extends UnitSpec {

  val allTopicVectorResults: Vector[TopicVectorInput] = MnMGen.getData()
  val p5 = ModelParams(5, 1, 1)
  val crp = new CRP(p5, allTopicVectorResults)

  "A VecMap" should {
    "offer a toMap method that returns a Map equal to the Map passed to its factory method" in {
      crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9)).toMap shouldEqual Map(1 -> 1, 2 -> 4, 3 -> 9)
    }
    "be equal to another VecMap created with an equal Map" in {
      crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9)) shouldEqual crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9))
    }
    "offer a size method that returns the size of the initializing Map" in {
      crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9, 5 -> 25)).size shouldEqual 4
      crp.VecMap(Map.empty).size shouldEqual 0
      crp.VecMap(Map(10 -> 9)).size shouldEqual 1
    }
    "offer a + method that combines sums (does Matrix addition on this one-row matrix)" in {
      val vecMap = crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9, 5 -> 25))
      vecMap + crp.VecMap(Map.empty) shouldEqual vecMap
      crp.VecMap(Map.empty) + vecMap shouldEqual vecMap
      vecMap + vecMap shouldEqual crp.VecMap(Map(1 -> 2, 2 -> 8, 3 -> 18, 5 -> 50))
      crp.VecMap(Map.empty) + crp.VecMap(Map.empty) shouldEqual crp.VecMap(Map.empty)
      vecMap + crp.VecMap(Map(77 -> 77, 88 -> 88, 99 -> 99)) shouldEqual crp.VecMap(Map(1 -> 1, 2 -> 4, 3 -> 9, 5 -> 25, 77 -> 77, 88 -> 88, 99 -> 99))
      vecMap + crp.VecMap(Map(77 -> 77, 88 -> 88, 99 -> 99)) shouldEqual crp.VecMap(Map(2 -> 4, 3 -> 9, 5 -> 25, 77 -> 77, 88 -> 88, 99 -> 99, 1 -> 1))
    }
  }
}


