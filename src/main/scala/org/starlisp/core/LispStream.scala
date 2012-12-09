package org.starlisp.core

import java.io.IOException

abstract trait LispStream extends LispObject {
  def eof: Boolean
  @throws(classOf[IOException]) def close: Boolean
}

abstract trait LispInputStream extends LispStream {
  @throws(classOf[IOException]) def read: LispObject
  @throws(classOf[IOException]) def readChar: LispChar
}

abstract trait LispOutputStream extends LispStream {
  @throws(classOf[IOException]) def write(str: String)
  @throws(classOf[IOException]) def write(ch: Char)
}