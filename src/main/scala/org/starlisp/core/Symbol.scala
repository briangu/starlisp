package org.starlisp.core

import java.util.concurrent.atomic.AtomicLong
import collection.mutable

class Environment(outer: Option[Environment] = None) {

  var index: Option[mutable.HashMap[String, Symbol]] = None

  def chain() : Environment = new Environment(Some(this))

  def gensym = new Symbol("G%d".format(Environment.genSymCounter.getAndIncrement()))

  def bind(sbl: Symbol, value: LispObject): Unit = index match {
    case Some(idx) => idx.getOrElseUpdate(sbl.name, sbl).value = value
    case None => {
      index = Some(new mutable.HashMap[String, Symbol])
      bind(sbl, value)
    }
  }

  def getSymbols: Cell = {
    /*
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
    */
    null
  }

  def isInterned(sym: Symbol) = find(sym.name) != None

  def find(symbol: Symbol): Option[Symbol] = find(symbol.name)
  def find(str: String): Option[Symbol] = index match {
    case Some(idx) =>  {
      idx.get(str) match {
        case Some(symbol) => Some(symbol)
        case None => outer match {
          case Some(outer) => outer.find(str)
          case None => None
        }
      }
    }
    case None => outer match {
      case Some(outer) => outer.find(str)
      case None => None
    }
  }

  def intern(symbol: Symbol): Symbol = index match {
    case Some(idx) => idx.getOrElseUpdate(symbol.name, symbol)
    case None => {
      index = Some(new mutable.HashMap[String, Symbol])
      intern(symbol)
    }
  }

  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(str: String, value: LispObject) : Symbol = intern(new Symbol(str, value))
}

object Environment {
  private val genSymCounter = new AtomicLong()

  val root = new Environment
}

object Symbol {

  private val env = Environment.root

  val internalError = intern("internal-error")
  val t: Symbol = intern("t")
  val standardOutput = intern("*standard-output*", new LispStreamImpl(null, System.out))
  val standardError = intern("*standard-error*", new LispStreamImpl(null, System.err))
  val lambda = intern("lambda")
  val quote = intern("quote")
  val _if = intern("if")
  val `macro` = intern("macro")
  val in = intern("in")
  val out = intern("out")

  Symbol.t.value = Symbol.t

  def intern(sym: Symbol): Symbol = env.intern(sym)
  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(str: String, value: LispObject) : Symbol = intern(new Symbol(str, value))
}

class Symbol(var name: String = null, var value: LispObject = null) extends LispObject {
  override def toString = name.toString //if (Symbol.isInterned(this)) name else "#:%s".format(name)
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = name.equals(obj)
}