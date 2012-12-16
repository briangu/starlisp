package org.starlisp.core

import java.io.{OutputStream, PrintWriter}

class LispOutputStreamImpl extends LispObject with LispOutputStream {

  private var out: PrintWriter = _
  private var open: Boolean = outputStreamP

  def this(os: OutputStream) {
    this()
    out = if (os != null) new PrintWriter(os, true) else null
  }

  def this(writer: PrintWriter) {
    this()
    out = writer
  }

  def close: Boolean = {
    if (open) {
      if (outputStreamP) out.close()
      open = false
      true
    } else {
      false
    }
  }

  def write(ch: Char) {
    out.print(ch)
    if (ch == '\n') out.flush()
  }

  def write(str: String) {
    out.print(str)
    out.flush()
  }

  def outputStreamP: Boolean = (out != null)
  def eof: Boolean = false

  override def toString: String = "#<" + super.toString + ">"
}