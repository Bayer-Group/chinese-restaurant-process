package com.monsanto.stats.tables.clustering

import com.monsanto.stats.tables._

case class MockRandomNumGen(values: Vector[Double]) extends RandomNumGen {
  var i = 0
  def next(): Double = {
    val result = values(i)
    i += 1
    if (i == values.length) i = 0
    result
  }
}

