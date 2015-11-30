package com.monsanto.stats.tables

import org.scalatest._
import scala.annotation.elidable
import elidable._

abstract class UnitSpec extends WordSpec with Matchers {
  // If the ASSERTION level is elided, any calls to assertionsEnabled will
  // be replaced with false
  @elidable(ASSERTION) def requirementsEnabled: Boolean = true
  def cancelIfRequirementElided(): Unit = {
    if (!requirementsEnabled)
      cancel("Can't perform this test because the requirement has been elided")
  }
}

