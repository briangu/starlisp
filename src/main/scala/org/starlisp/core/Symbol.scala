package org.starlisp.core

import java.util.concurrent.atomic.AtomicLong
import collection.mutable

class Environment(outer: Option[Environment] = None) {

  val index = new mutable.HashMap[String, Symbol]

  override def toString(): String = index map {case (k,v) => "%s => %s".format(k,v)} mkString("\n")

  def printChain() {
    println("--->")
    println(toString())
    outer match {
      case Some(env) => env.printChain()
      case None => ;
    }
  }

  def chain() : Environment = {
   // printChain()
    new Environment(Some(this))
  }

  def gensym = new Symbol("G%d".format(Environment.genSymCounter.getAndIncrement()))

  def bind(sbl: Symbol, value: LispObject) = index.getOrElseUpdate(sbl.name, sbl).value = value

  def getSymbols: Cell = {
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
  }

  def isInterned(sym: Symbol) = find(sym.name) != None

  def find(symbol: Symbol): Option[Symbol] = find(symbol.name)
  def find(str: String): Option[Symbol] = {
    index.get(str) match {
      case Some(symbol) => Some(symbol)
      case None => outer match {
        case Some(outer) => outer.find(str)
        case None => None
      }
    }
  }

  def internChained(symbol: Symbol): Option[Symbol] = Option(find(symbol)) match {
    case Some(x) => x
    case None => Some(index.getOrElseUpdate(symbol.name, symbol))
  }
  def intern(symbol: Symbol): Symbol = index.getOrElseUpdate(symbol.name, symbol)
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