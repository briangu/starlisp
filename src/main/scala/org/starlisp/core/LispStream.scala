package org.starlisp.core

import java.io.{PrintWriter, StringWriter, IOException}

trait LispStream extends LispObject {
  def eof: Boolean
  @throws(classOf[IOException]) def close: Boolean
}

trait LispInputStream extends LispStream {
  @throws(classOf[IOException]) def read: LispObject
  @throws(classOf[IOException]) def readChar: LispChar
}

trait LispOutputStream extends LispStream {
  @throws(classOf[IOException]) def write(str: String)
  @throws(classOf[IOException]) def write(ch: Char)
}

class StringOutputStream(writer: StringWriter = new StringWriter) extends LispOutputStreamImpl(new PrintWriter(writer)) {
  def getOutputStreamString: String = {
    val sb = writer.getBuffer
    val result: String = sb.toString
    sb.setLength(0)
    result
  }
}

