package com.monsanto.stats.tables.utils

object ProfileUtils {

  def time[T](task: => T): (T, Long) = {
    val start = scala.compat.Platform.currentTime
    val result: T = task
    val end = scala.compat.Platform.currentTime
    (result, end - start)
  }

  // Chee Seng, in a hurry so didn't have time to fix yours. If
  // you are OK with this, can use this instead of the first one
  // above. Looks prettier in client code.
  def timing[T](name: String, printTime: Boolean = false)(task: => T): T = {
    val start = scala.compat.Platform.currentTime
    val result: T = task
    val end = scala.compat.Platform.currentTime
    if (printTime) println(name + ": " + (end - start))
    result
  }
}
