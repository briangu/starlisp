package org.starlisp.core

import java.util.concurrent.atomic.AtomicLong

trait Environment {
  def getSymbols: Cell

  def chain: Environment
  def depth(x: Int = 0): Int

  def bind(sbl: Symbol, value: LispObject)

  def find(symbol: Symbol): Option[Symbol]
  def find(str: String): Option[Symbol]

  def isInterned(sym: Symbol) = find(sym.name) != None

  def intern(symbol: Symbol): Symbol
  def intern(str: String): Symbol = intern(new Symbol(str))
  def intern(str: String, value: LispObject) : Symbol = intern(new Symbol(str, value))
}

object RootEnvironment extends Environment {

  var index = new collection.mutable.HashMap[String, Symbol]

  def chain: Environment = new LexicalEnvironment(this)
  def depth(x: Int) = x + 1

  def gensym = Symbol.gensym

  def bind(sbl: Symbol, value: LispObject) {
    index.getOrElseUpdate(sbl.name, sbl).value = value
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

  def find(symbol: Symbol): Option[Symbol] = find(symbol.name)
  def find(str: String): Option[Symbol] = index.get(str)

  def intern(symbol: Symbol): Symbol = index.getOrElseUpdate(symbol.name, symbol)
}

class LexicalEnvironment(var proxy: Environment) extends Environment {

  class ActiveEnvironment(outer: Environment) extends Environment {
    var index = new collection.mutable.HashMap[String, Symbol]

    override def find(str: String) = {
      val x = index.get(str)
      if (x eq None) {
        outer.find(str)
      } else {
        x
      }
    }

    def getSymbols = throw new UnsupportedOperationException
    def chain = throw new UnsupportedOperationException
    def depth(x: Int) = outer.depth(x + 1)
    def bind(sbl: Symbol, value: LispObject) { throw new UnsupportedOperationException }
    def find(symbol: Symbol) = throw new UnsupportedOperationException
    def intern(symbol: Symbol) = throw new UnsupportedOperationException
  }

  private def getWritableRouter: ActiveEnvironment = {
    if (!proxy.isInstanceOf[ActiveEnvironment]) {
      proxy = new ActiveEnvironment(proxy)
    }
    proxy.asInstanceOf[ActiveEnvironment]
  }

  // TODO: preallocate environments in an array for cache locality?
  def chain: Environment = new LexicalEnvironment(this)
  def depth(x: Int = 0) = proxy.depth(x + 1)

  def bind(sbl: Symbol, value: LispObject) {
    getWritableRouter.index.getOrElseUpdate(sbl.name, new Symbol(sbl.name, sbl.value)).value = value
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

  def find(symbol: Symbol): Option[Symbol] = proxy.find(symbol.name)
  def find(str: String): Option[Symbol] = proxy.find(str)

  def intern(symbol: Symbol): Symbol = getWritableRouter.index.getOrElseUpdate(symbol.name, symbol)
}

object Symbol {

  private val env = RootEnvironment

  private val genSymCounter = new AtomicLong()

  def gensym = new Symbol("G%d".format(genSymCounter.getAndIncrement))

  val internalError = intern("internal-error")
  val t: Symbol = intern("t")
  val standardOutput = intern("*standard-output*", new LispOutputStreamWriter(System.out))
  val standardError = intern("*standard-error*", new LispOutputStreamWriter(System.err))

  val lambda = intern(new Procedure("lambda") {
    def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      head
    }
  })

  val `macro` = intern(new Procedure("macro") {
    def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      head
    }
  })

  val quote = intern(new Procedure("quote") {
    def apply(env: Environment, list: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      list.cadr
    }
  })

  val _if = intern(new Procedure("if") {
    def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      val list = head.rest
      Option(eval(list.car, env)) match {
        case Some(_) => eval(list.cadr, env)
        case None => Option(list.cddr) match {
          case Some(cell) => eval(cell.car, env)
          case None => null
        }
      }
    }
  })

  val in = intern("in")
  val out = intern("out")

  Symbol.t.value = Symbol.t

  private def intern(proc: Procedure): Symbol = env.intern(proc.name, proc)
  private def intern(sym: Symbol): Symbol = env.intern(sym)
  private def intern(str: String): Symbol = intern(new Symbol(str))
  private def intern(str: String, value: LispObject) : Symbol = intern(new Symbol(str, value))
}

class Symbol(var name: String = null, var value: LispObject = null) extends LispObject {
  override def toString = name.toString //if (Symbol.isInterned(this)) name else "#:%s".format(name)
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = {
    obj match {
      case sym: Symbol => name.equals(sym.name)
      case _ => false
    }
  }
}