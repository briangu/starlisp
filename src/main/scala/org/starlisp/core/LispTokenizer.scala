package org.starlisp.core

import java.io._

class LispTokenizer(in: Reader, out: PrintWriter) extends LispObject with LispStream {

  def this(is: InputStream, os: OutputStream) {
    this(if (is != null) new InputStreamReader(is, "UTF-8") else null,
         if (os != null) new PrintWriter(os, true) else null)
  }

  val tokenizer = if (in != null) new FastStreamTokenizer(in) else null

  def next() : Int = {
    try {
      tokenizer.nextToken()
    } catch {
      case e: Exception => StreamTokenizer.TT_EOF
    }
  }

  private def dispatch() : LispObject = {
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
          new LispArray(readList().asInstanceOf[Cell]) // TODO: add list check
        }
        case ch => {
          throw new LispException("dispatch syntax error.")
        }
      }
    } finally {
      tokenizer.useSExprSyntaxMode()
    }
  }

  case class ListEnd() extends LispObject
  class LispDottedCdr(var obj: LispObject = null) extends LispObject {}

  private val listEnd = new ListEnd
  private val dottedCdr = new LispDottedCdr

  def readList() : LispObject = {
    var obj = read()
    if (obj == listEnd) {
      null
    } else {
      var cell = new Cell(obj)
      val list = cell
      obj = read()
      if (obj != listEnd) {
        do {
          if (obj.isInstanceOf[LispDottedCdr]) {
            cell.cdr = obj.asInstanceOf[LispDottedCdr].obj
          } else {
            cell.cdr = new Cell(obj)
            cell = cell.cdr.asInstanceOf[Cell]
          }
          obj = read
        } while (obj != listEnd)
      }
      list
    }
  }

  def readWord() : LispObject = {
    val str = String.copyValueOf(tokenizer.buf, 0, tokenizer.bufLimit)
    if (str.equals(".")) {
      dottedCdr.obj = read
      dottedCdr
    } else {
      // TODO: make more efficient
      if (LispNumber.isNumber(str)) LispNumber.parse(str) else Symbol.intern(str)
    }
  }

  def readQuotedSymbol() : Symbol = {
    try {
      tokenizer.useCharReadMode()
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

  def read() : LispObject = {
     next match {
      case '(' => readList()
      case StreamTokenizer.TT_WORD => readWord()
      case ')' => listEnd
      case '\'' => new Cell(Symbol.quote, new Cell(read, null))
      case '"' => new LispString(tokenizer.buf, tokenizer.bufLimit)
      case '#' => dispatch()
      case '|' => readQuotedSymbol()
      case StreamTokenizer.TT_EOF => null
      case ttype => throw new RuntimeException("unhandled type: " + ttype)
    }
  }

  def writeJavaString(str: String) {}

  def writeJavaChar(ch: Char) {}

  def eof() = false

  def close() = false

  def readChar() = null
}
