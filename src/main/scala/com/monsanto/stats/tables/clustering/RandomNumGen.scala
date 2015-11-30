package com.monsanto.stats.tables.clustering

trait RandomNumGen {
  // Generates a positive Double between 0.0 and 1.0.
  def next(): Double
}

object RealRandomNumGen extends RandomNumGen {
  def next(): Double = Math.random()
}

object SameRandomNumGen extends RandomNumGen {
  val rnd = new scala.util.Random(987898798L)
  def next(): Double = rnd.nextDouble()
}  
