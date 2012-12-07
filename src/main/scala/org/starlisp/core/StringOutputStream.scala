package org.starlisp.core

import java.io.PrintWriter
import java.io.StringWriter

class StringOutputStream(writer: StringWriter = new StringWriter) extends LispStreamImpl(null, new PrintWriter(writer)) {
  def getOutputStreamString: String = {
    val sb = writer.getBuffer
    val result: String = sb.toString
    sb.setLength(0)
    result
  }
}