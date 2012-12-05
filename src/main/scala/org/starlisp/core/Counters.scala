package org.starlisp.core

import collection.mutable
import java.util.concurrent.atomic.AtomicInteger

object Counters {

  private val cache = new mutable.HashMap[String, AtomicInteger]

  def inc(str: String) {
    cache.getOrElseUpdate(str, new AtomicInteger()).incrementAndGet()
  }

  def report() {
    cache.foreach{case (key, value) => println("%s -> %d".format(key, value.get()))}
  }
}
