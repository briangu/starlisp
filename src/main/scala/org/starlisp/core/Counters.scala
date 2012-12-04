package org.starlisp.core

import collection.mutable

object Counters {

  private val cache = new mutable.HashMap[String, Int]

  def inc(str: String) {
    cache.put(str, cache.getOrElseUpdate(str, 0) + 1)
  }

  def report() {
    cache.foreach{case (key, value) => println("%s -> %d".format(key, value))}
  }
}
