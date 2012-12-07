package org.starlisp.core

import java.io._

class LispTokenizer(in: Reader, out: PrintWriter) extends LispObject with LispInputStream {

  case class ListEnd() extends LispObject
  class LispDottedCdr(var obj: LispObject = null) extends LispObject {}

  val tokenizer = if (in != null) new FastStreamTokenizer(in) else null

  private val listEnd = new ListEnd
  private val dottedCdr = new LispDottedCdr

  private var atEOF = false

  def this(is: InputStream, os: OutputStream) {
    this(if (is != null) new InputStreamReader(is, "UTF-8") else null,
         if (os != null) new PrintWriter(os, true) else null)
  }

  def eof() = atEOF
  def readChar() = throw new UnsupportedOperationException
  def close(): Boolean = {
    in.close()
    atEOF = true
    true
  }

  def next(): Int = {
    try {
      tokenizer.nextToken()
    } catch {
      case e: Exception => StreamTokenizer.TT_EOF
    }
  }

  private def dispatch(): LispObject = {
    tokenizer.useCharReadMode()
    try {
      tokenizer.nextToken match {
        case ';' => {
          tokenizer.useSExprSyntaxMode()
          read() // skip next s-exp
          null
        }
        case '\\' => {
          tokenizer.nextToken()
          LispChar.create(tokenizer.ttype.asInstanceOf[Char])
        }
        case '(' => {
          tokenizer.useSExprSyntaxMode()
          new LispArray(readList().asInstanceOf[Cell])
        }
        case '\'' => read()
        case ch => throw new LispException("dispatch syntax error for: " + String.valueOf(ch))
      }
    } finally {
      tokenizer.useSExprSyntaxMode()
    }
  }

  def readList(): LispObject = {
    var obj = read()
    if (obj eq listEnd) {
      null
    } else {
      var cell = new Cell(obj)
      val list = cell
      obj = read()
      if (obj ne listEnd) {
        do {
          if (obj eq dottedCdr) {
            cell.cdr = dottedCdr.obj
          } else {
            cell.cdr = new Cell(obj)
            cell = cell.cdr.asInstanceOf[Cell]
          }
          obj = read()
        } while (obj ne listEnd)
      }
      list
    }
  }

  def readWord(): LispObject = {
    val str = String.copyValueOf(tokenizer.buf, 0, tokenizer.bufLimit)
    if (str.length == 1 && str.equals(".")) {
      dottedCdr.obj = read
      dottedCdr
    } else {
      // TODO: make more efficient
      if (LispNumber.isNumber(str)) LispNumber.tryParse(str) else Symbol.intern(str)
    }
  }

  def readQuotedSymbol(): Symbol = {
    tokenizer.useCharReadMode()
    try {
      tokenizer.nextToken()
      val sb = new java.lang.StringBuilder(1)
      while (tokenizer.ttype != '|' && tokenizer.ttype != StreamTokenizer.TT_EOF) {
        sb.append(tokenizer.buf(0))
        tokenizer.nextToken()
      }
      val symStr = sb.toString
      if (symStr.equals("nil")) null else Symbol.intern(symStr);
    } finally {
      tokenizer.useSExprSyntaxMode()
    }
  }

  def read(): LispObject = {
     next match {
      case '(' => readList()
      case StreamTokenizer.TT_WORD => readWord()
      case ')' => listEnd
      case '\'' => new Cell(Symbol.quote, new Cell(read(), null))
      case '"' => new LispString(tokenizer.buf, tokenizer.bufLimit)
      case '#' => dispatch()
      case '|' => readQuotedSymbol()
      case StreamTokenizer.TT_EOF => {
        atEOF = true
        null
      }
      case ttype => throw new RuntimeException("unhandled type: " + ttype.asInstanceOf[Char])
    }
  }
}
