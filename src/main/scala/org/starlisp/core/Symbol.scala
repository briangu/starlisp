package org.starlisp.core

import java.io.UnsupportedEncodingException
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

  def gensym = Symbol.gensym

/*
  private val DEFAULT_STACK_SIZE = 32768 * 2
  private var stackSize = 0
  private val stack = new Array[LispObject](DEFAULT_STACK_SIZE)

  def save { stackSize += 1 }
  def restore {
    stackSize -= 1
    while (stack(stackSize) != null) {
      (stack(stackSize).asInstanceOf[Symbol]).value = stack(stackSize - 1)
      stack(stackSize) = null
      stack(stackSize - 1) = null
      stackSize -= 2
    }
  }

  def bind(sbl: Symbol, value: LispObject) {
    val oldValue: LispObject = sbl.value
    sbl.value = value
    var i = stackSize - 1
    while (stack(i) != null) {
      if (stack(i) eq sbl) return
      i -= 2
    }
    stack(stackSize) = oldValue
    stackSize += 1;
    stack(stackSize) = sbl
    stackSize += 1;
  }
*/
  def bind(sbl: Symbol, value: LispObject) = index.getOrElseUpdate(sbl.name, sbl).value = value

  def getSymbols: Cell = {
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
  }

  def isInterned(sym: Symbol) = find(sym.name) != null

  def find(str: String): Symbol = {
    index.get(str) match {
      case Some(symbol) => symbol
      case None => outer match {
        case Some(outer) => outer.find(str)
        case None => null
      }
    }
  }

  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(sym: Symbol): Symbol = index.getOrElseUpdate(sym.name, sym)
}

object Environment {
  val root = new Environment
}

object Symbol {

  private val env = Environment.root

  private val genSymCounter = new AtomicLong()

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

  def gensym = new Symbol("G%d".format(genSymCounter.getAndIncrement()))

  def chain() = env.chain()

  def isInterned(sym: Symbol) = env.isInterned(sym)

  def intern(sym: Symbol) : Symbol = env.intern(sym)
  def intern(str: String) : Symbol = intern(new Symbol(str))
}

class Symbol(var name: String = null) extends LispObject {

  var value: LispObject = null

  override def toString = if (Symbol.isInterned(this)) name else "#:%s".format(name)
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = name.equals(obj)
}