package org.starlisp.core

import collection.mutable.HashMap
import java.io.UnsupportedEncodingException

class SymbolContext {

  val index = new HashMap[String, Symbol]

  def getSymbols: Cell = {
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
  }

  def findSymbol(str: String) = index.getOrElse(str, null)

  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(sym: Symbol): Symbol = Symbol.intern(this, sym)
}

object Symbol {

  private val context = new SymbolContext

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
    Symbol.standardOutput.value = new LispStreamImpl(null, System.out)
    Symbol.standardInput.value = new LispStreamImpl(System.in, null)
    Symbol.standardError.value = new LispStreamImpl(null, System.err)
  }
  catch {
    case e: UnsupportedEncodingException => ;
  }

  // TODO: actually clone
  def cloneSymbols() : SymbolContext = context

  def intern(sym: Symbol) : Symbol = intern(context, sym)
  def intern(str: String) : Symbol = intern(new Symbol(str))

  def intern(context: SymbolContext, sym: Symbol): Symbol = {
    if (sym.interned) {
      sym
    } else {
      val sbl = context.findSymbol(sym.name)
      if (sbl == null) {
        context.index(sym.name) = sym
        sym.interned = true // TODO: can we just return sbl and throw away sym?
        sym
      } else {
        sbl
      }
    }
  }
}

class Symbol(var name: String = null) extends LispObject {

  var value: LispObject = null
  private var interned: Boolean = false

  override def toString: String = if (this.interned) this.name else "#:" + this.name
}