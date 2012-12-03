package org.starlisp.core

import java.io._


class LispTokenizer(in: Reader, out: PrintWriter) extends LispObject with LispStream {

  def this(is: InputStream, os: OutputStream) {
    this(if (is != null) new BufferedReader(new InputStreamReader(is, "UTF-8")) else null,
         if (os != null) new PrintWriter(os, true) else null)
  }

  val tokenizer : StreamTokenizer = if (in != null) {
    val tok = new StreamTokenizer(in)
    tok.resetSyntax()
    tok.whitespaceChars(0, ' ')
    tok.wordChars(' '+1,255)
    tok.ordinaryChar('(')
    tok.ordinaryChar(')')
    tok.ordinaryChar('\'')
    tok.ordinaryChar('#')
    tok.ordinaryChar('.')
//    tok.parseNumbers()
    tok.commentChar(';')
    tok.quoteChar('"')
    tok
  } else {
    null
  }

  def next() : Int = {
    try {
      tokenizer.nextToken()
    } catch {
      case e: Exception => StreamTokenizer.TT_EOF
    }
  }

  def read() : LispObject = {
     next match {
      case '(' => {
        var a = read
        val cell = new Cell
        var currentCell = cell
        while (a != null) {
          if (currentCell.car != null) {
            val cdr = new Cell
            currentCell.cdr = cdr
            currentCell = cdr
          }
          currentCell.car = a
          a = read
        }
        cell
      }
      case StreamTokenizer.TT_WORD => {
//        if (tokenizer.sval.equals("nil")) null else Symbol.intern(tokenizer.sval);
        //Symbol.intern(tokenizer.sval)
        val str = tokenizer.sval
        if (LispNumber.javaStringMatchesLispNumber(str))          // Is a number
          LispNumber.parse(str);
        else
          Symbol.intern(str);
      }
      case StreamTokenizer.TT_NUMBER => {
        val fixNum = math.round(tokenizer.nval)
        if (fixNum == tokenizer.nval) {
          new LispFixnum(fixNum)
        } else {
          // TODO: bignum
          new LispFlonum(tokenizer.nval)
        }
      }
      case ')' => null
      case '\'' => new Cell(Symbol.quote, new Cell(read, null))
      case '"' => new LispString(tokenizer.sval)
      case '.' => new Cell(read) // TODO: fix
      case '#' => null // TODO: dispatch
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
