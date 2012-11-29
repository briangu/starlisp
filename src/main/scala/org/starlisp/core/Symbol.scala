package org.starlisp.core

import collection.mutable.HashMap

object Symbol {

  private var symbols: Cons = null
  private val index = new HashMap[String, Symbol]

  def getSymbols: Cons = symbols

  def findSymbol(str: String): Symbol = index.getOrElse(str, null)

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