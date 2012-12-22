package org.starlisp.core

import java.util.concurrent.atomic.AtomicLong

class Environment(outer: Option[Environment] = None) {

  trait RouterEnvironment {
    def find(symbol: Symbol): Option[Symbol] = find(symbol.name)
    def find(str: String): Option[Symbol]
  }

  class EmptyEnvironment() extends RouterEnvironment {
    def find(str: String) = None
  }

  class EmptyEnvironment2(outer: Option[Environment]) extends RouterEnvironment {
    val env = outer.get
    def find(str: String) = env.find(str)
  }

  class ActiveEnvironment extends RouterEnvironment {
    var index = new collection.mutable.HashMap[String, Symbol]
    def find(str: String) = index.get(str)
  }

  class ActiveEnvironment2(outer: Option[Environment]) extends ActiveEnvironment {
    val env = outer.get
    override def find(str: String) = {
      val x = index.get(str)
      if (x eq None) {
        env.find(str)
      } else {
        x
      }
    }
  }

  private def getWritableRouter: ActiveEnvironment = {
    router match {
      case x: EmptyEnvironment => {createActiveRouter; router.asInstanceOf[ActiveEnvironment]}
      case x: EmptyEnvironment2 => {createActiveRouter; router.asInstanceOf[ActiveEnvironment]}
      case _ => router.asInstanceOf[ActiveEnvironment]
    }
  }
  private def createActiveRouter {
    router = if (outer eq None) new ActiveEnvironment() else new ActiveEnvironment2(outer)
  }

  var router = if (outer eq None) new EmptyEnvironment else new EmptyEnvironment2(outer)

  def chain() : Environment = new Environment(Some(this))

  def gensym = new Symbol("G%d".format(Environment.genSymCounter.getAndIncrement()))

  def bind(sbl: Symbol, value: LispObject): Unit = {
    getWritableRouter.index.getOrElseUpdate(sbl.name, sbl).value = value
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

  def find(symbol: Symbol): Option[Symbol] = router.find(symbol.name)
  def find(str: String): Option[Symbol] = router.find(str)
  def intern(symbol: Symbol): Symbol = getWritableRouter.index.getOrElseUpdate(symbol.name, symbol)

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
  val standardOutput = intern("*standard-output*", new LispOutputStreamWriter(System.out))
  val standardError = intern("*standard-error*", new LispOutputStreamWriter(System.err))
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