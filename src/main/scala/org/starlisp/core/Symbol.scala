package org.starlisp.core

import java.io.UnsupportedEncodingException
import java.util.concurrent.atomic.AtomicLong

class Environment(outer: Option[Environment] = None) {

  val index = new java.util.HashMap[String, Symbol](1024)

  private val DEFAULT_STACK_SIZE = 32768 * 2
  private var stackSize = 0
  private val stack = new Array[LispObject](DEFAULT_STACK_SIZE)

  // TODO: chain
  def chain() : Environment = this

  def gensym = Symbol.gensym

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

  def getSymbols: Cell = {
    import scala.collection.JavaConversions._
    var symbols: Cell = null
    index.foreach{ case (name, sym) =>
      symbols = new Cell(sym, symbols)
    }
    symbols
  }

  def isInterned(sym: Symbol) = index.containsKey(sym.name)

  def find(str: String): Symbol = {
    Option(index.get(str)) match {
      case Some(symbol) => symbol
      case None => outer match {
        case Some(outer) => outer.find(str)
        case None => null
      }
    }
  }

  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(sym: Symbol): Symbol = Symbol.intern(this, sym)
}

object Symbol {

  private val env = new Environment

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

  def intern(sym: Symbol) : Symbol = intern(env, sym)
  def intern(str: String) : Symbol = intern(new Symbol(str))
  def intern(context: Environment, sym: Symbol): Symbol = {
    val sbl = context.find(sym.name)
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