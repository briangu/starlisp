package org.starlisp.core

import collection.mutable.HashMap
import java.io.UnsupportedEncodingException

object Symbol {

  private var symbols: Cons = null
  private val index = new HashMap[String, Symbol]

  val internalError = intern("internal-error")
  val t: Symbol = intern("t")
  val standardOutput = intern("*standard-output*")
  val standardInput = intern("*standard-input*")
  val standardError = intern("*standard-error*")
  val lambda = intern("lambda")
  val quote = intern("quote")
  val _if = intern("if")
  val `macro` = intern("macro")
  val in = intern("in")
  val out = intern("out")

  Symbol.t.value = Symbol.t
  try {
    Symbol.standardOutput.value = new LispStream(null, System.out)
    Symbol.standardInput.value = new LispStream(System.in, null)
    Symbol.standardError.value = new LispStream(null, System.err)
  }
  catch {
    case e: UnsupportedEncodingException => ;
  }

  def getSymbols: Cons = symbols

  def findSymbol(str: String) = index.getOrElse(str, null)

  def intern(str: String): Symbol = (new Symbol(str)).intern

  private def findSymbol(str: String, list: Cons): Symbol = {
    if (list == null) {
      null
    } else if (str == (list.car.asInstanceOf[Symbol]).str) {
      list.car.asInstanceOf[Symbol]
    } else {
      findSymbol(str, list.cdr.asInstanceOf[Cons])
    }
  }
}

class Symbol extends LispObject {

  var value: LispObject = null
  private var str: String = null
  private var interned: Boolean = false

  def this(str: String) {
    this()
    this.str = str
    this.interned = false
  }

  def intern: Symbol = {
    if (this.interned) return this
    val sbl = Symbol.findSymbol(this.str)
    if (sbl == null) {
      Symbol.symbols = new Cons(this, Symbol.symbols)
      Symbol.index(str) = this
      this.interned = true
      this
    } else {
      sbl
    }
  }

  def getStr: String = this.str

  override def toString: String = {
    if (this.interned) this.str else "#:" + this.str
  }
}