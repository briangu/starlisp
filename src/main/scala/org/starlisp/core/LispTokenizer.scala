package org.starlisp.core

import java.io._

class LispDottedCdr(val obj: LispObject) extends LispObject {}

class LispTokenizer(in: Reader, out: PrintWriter) extends LispObject with LispStream {

  def this(is: InputStream, os: OutputStream) {
    this(if (is != null) new BufferedReader(new InputStreamReader(is, "UTF-8")) else null,
         if (os != null) new PrintWriter(os, true) else null)
  }

  val tokenizer : FastStreamTokenizer = if (in != null) {
    val tok = new FastStreamTokenizer(in)
    tok.resetSyntax()
    setSyntax(tok)
    tok
  } else {
    null
  }

  private var syntaxIsSet = false
  private def resetSyntax() = {
    tokenizer.resetSyntax()
    syntaxIsSet = false
  }
  private def setSyntax(tok: FastStreamTokenizer = tokenizer) {
    if (syntaxIsSet) return
    tok.whitespaceChars(0, ' ')
    tok.wordChars(' '+1,255)
    tok.ordinaryChar('(')
    tok.ordinaryChar(')')
    tok.ordinaryChar('\'')
    tok.ordinaryChar('#')
    tok.commentChar(';')
    tok.quoteChar('"')
    syntaxIsSet = true
  }

  def next() : Int = {
    try {
      tokenizer.nextToken()
    } catch {
      case e: Exception => StreamTokenizer.TT_EOF
    }
  }

  private def dispatch() : LispObject = {
    resetSyntax()
    try {
      tokenizer.nextToken match {
        case ';' => {
          setSyntax()
          read() // skip next s-exp
          null
        }
        case '\\' => {
          tokenizer.nextToken()
          LispChar.create(tokenizer.ttype.asInstanceOf[Char])
        }
        case '(' => {
          tokenizer.pushBack()
          new LispArray(read().asInstanceOf[Cell]) // TODO: add list check
        }
        case ch => {
          tokenizer.pushBack()
          null
        }
      }
    } finally {
      setSyntax()
    }
  }

  case class ListEnd() extends LispObject

  private val listEnd = new ListEnd

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
    val str = tokenizer.sval
    if (str.equals(".")) {
      new LispDottedCdr(read)
    } else {
      // TODO: make more efficient
      if (LispNumber.isNumber(str)) LispNumber.parse(str) else Symbol.intern(str)
    }
  }

  def read() : LispObject = {
     next match {
      case '(' => readList()
      case StreamTokenizer.TT_WORD => readWord()
      case ')' => listEnd
      case '\'' => new Cell(Symbol.quote, new Cell(read, null))
      case '"' => new LispString(tokenizer.sval)
      case '#' => dispatch()
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
