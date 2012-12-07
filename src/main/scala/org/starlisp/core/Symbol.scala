package org.starlisp.core

import java.io.UnsupportedEncodingException

class SymbolContext {

  val index = new java.util.HashMap[String, Symbol](1024)

  def getSymbols: Cell = {
    import scala.collection.JavaConversions._
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
  }

  def isInterned(sym: Symbol) = index.containsKey(sym.name)
  def findSymbol(str: String) = index.get(str)

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
    Symbol.standardInput.value = new LispTokenizer(System.in, null)
    Symbol.standardError.value = new LispStreamImpl(null, System.err)
  }
  catch {
    case e: UnsupportedEncodingException => ;
  }

  // TODO: actually clone
  def cloneSymbols() : SymbolContext = context

  def isInterned(sym: Symbol) = context.isInterned(sym)

  def intern(sym: Symbol) : Symbol = intern(context, sym)
  def intern(str: String) : Symbol = intern(new Symbol(str))
  def intern(context: SymbolContext, sym: Symbol): Symbol = {
    val sbl = context.findSymbol(sym.name)
    if (sbl == null) {
      context.index.put(sym.name, sym)
      sym
    } else {
      sbl
    }
  }
}

class Symbol(var name: String = null) extends LispObject {

  var value: LispObject = null

  override def toString = if (Symbol.isInterned(this)) name else "#:%s".format(name)
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = name.equals(obj)
}