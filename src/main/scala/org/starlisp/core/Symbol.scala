package org.starlisp.core

import java.util.concurrent.atomic.AtomicLong
import java.util.Date

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

trait RouterEnvironment {
  def find(symbol: Symbol): Option[Symbol] = find(symbol.name)
  def find(str: String): Option[Symbol]
}

class EmptyEnvironment(outer: Environment) extends RouterEnvironment {
  def find(str: String) = outer.find(str)
}

class ActiveEnvironment(outer: Environment) extends RouterEnvironment {
  var index = new collection.mutable.HashMap[String, Symbol]
  override def find(str: String) = {
    val x = index.get(str)
    if (x eq None) {
      outer.find(str)
    } else {
      x
    }
  }
}

class LexicalEnvironment(outer: Environment) extends Environment {

  var router: RouterEnvironment = new EmptyEnvironment(outer)

  private def getWritableRouter: ActiveEnvironment = {
    router match {
      case r: EmptyEnvironment => { val e = new ActiveEnvironment(outer); router = e; e }
      case r: ActiveEnvironment => r
    }
  }

  def chain: Environment = new LexicalEnvironment(this)
  def depth(x: Int = 0) = outer.depth(x + 1)

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

  def find(symbol: Symbol): Option[Symbol] = router.find(symbol.name)
  def find(str: String): Option[Symbol] = router.find(str)

  def intern(symbol: Symbol): Symbol = getWritableRouter.index.getOrElseUpdate(symbol.name, symbol)
}

object Symbol {

  private val env = RootEnvironment

  private val genSymCounter = new AtomicLong()

  def gensym = new Symbol("G%d".format(genSymCounter.getAndIncrement()))

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

  val nil = null
  //val t = Symbol.t
  type Args = Array[LispObject]

  private def intern(proc: Procedure): Unit = {
    intern(proc.name).value = proc
  }

  private def eq(obj1: LispObject, obj2: LispObject) = if (obj1 == obj2) t else nil
  private def eql(a: LispObject, b: LispObject): LispObject = {
    if ((a eq nil) || (b eq nil))
      eq(a, b)
    else if (!a.getClass.isInstance(b))
      nil
    else if (a.isInstanceOf[LispChar])
      if (a.as[LispChar].ch == a.as[LispChar].ch) t else nil
    else if ((a.isInstanceOf[LispNumber]))
      if (a.as[LispNumber] == b.as[LispNumber]) t else nil
    else
      eq(a, b)
  }

  /*
   * Define the global "keywords" of the system.
   * At the moment, this is done largely for performance as the symbol value is already defined
   * and environment chaining isn't needed.  To enable a symbol to be redefined by an application,
   * just move it out of here and back to Runtime.
   *
   * TODO: define these via a lisp file similar to how jscheme does it.
   *
   */

  intern(new Procedure("env-depth") {
    def apply(env: Environment, head: Cell, eval: (LispObject, Environment) => LispObject) = {
      LispFixnum.create(env.depth(0))
    }
  })

  intern(new Procedure(Symbol.lambda.name) {
    def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      head
    }
  })

  intern(new Procedure(Symbol.`macro`.name) {
    def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      head
    }
  })

  intern(new Procedure(Symbol._if.name) {
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
  intern("nil").value = nil
  intern("Class").value = new JavaObject(classOf[java.lang.Class[_]])

  intern(new Procedure("aeq"){
    def apply(env: Environment, list: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      val evalA = eval(list.cadr, env)
      val evalB = eval(list.caddr, env)
      val areEqual = if (evalA.getClass == evalB.getClass) {
        (evalA, evalB) match {
          case (a: Cell, b: Cell) => a.toString.equals(b.toString)
          case (a, b) => a == b
        }
      } else {
        false
      }
      if (!areEqual) error("%s not equal %s".format(evalA,evalB))
      t
    }
  })

  intern(new Procedure(Symbol.quote.name) {
    def apply(env: Environment, list: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
      list.cadr
    }
  })

  intern(new LispFnP[LispObject]("cons") {
    def apply(a: LispObject, b:LispObject) = { new Cell(a, b) }
  })
  intern(new LispFn1[Cell]("car") {
    def apply(a: Cell) = { if (a eq nil) nil else a.car }
  })
  intern(new LispFn1[Cell]("cdr") {
    def apply(a: Cell) = { if (a eq nil) nil else a.cdr }
  })
  intern(new LispFn2[Cell, LispObject]("rplaca") {
    def apply(a: Cell, b: LispObject) = { a.Car(b); a }
  })
  intern(new LispFn2[Cell, LispObject]("rplacd") {
    def apply(a: Cell, b: LispObject) = { a.Cdr(b); a }
  })
  intern(new LispFn1[Symbol]("symbol-value") {
    def apply(a: Symbol) = { if (a eq nil) nil else a.value }
  })
  intern(new LispFn1[LispObject]("intern") {
    def apply(a: LispObject) = {
      if (a.isInstanceOf[LispString]) intern((a.as[LispString]).toJavaString)
      else if (a.isInstanceOf[Symbol]) Symbol.intern(a.as[Symbol])
      else throw new LispException(Symbol.internalError, "Bad argument")
    }
  })
  intern(new LispFnP[LispObject]("eq?") {
    def apply(a: LispObject, b:LispObject) = { if (a == b) t else nil }
  })
  intern(new LispFn1[LispObject]("atom?") {
    def apply(a: LispObject) = if (a.isInstanceOf[Cell]) nil else t
  })
  intern(new LispFnP[LispNumber]("+") {
    def apply(a: LispNumber, b: LispNumber) = { a.add(b) }
  })
  intern(new LispFnP[LispNumber]("-") {
    def apply(a: LispNumber, b: LispNumber) = { a.sub(b) }
  })
  intern(new LispFnP[LispNumber]("*") {
    def apply(a: LispNumber, b: LispNumber) = { a.mul(b) }
  })
  intern(new LispFnP[LispNumber]("/") {
    def apply(a: LispNumber, b: LispNumber) = { a.div(b) }
  })
  intern(new LispFnP[LispInteger]("mod") {
    def apply(a: LispInteger, b: LispInteger) = { a.mod(b) }
  })
  intern(new LispFnP[LispInteger]("ash") {
    def apply(a: LispInteger, b: LispInteger) = { a.ash(b) }
  })
  intern(new LispFn1[LispNumber]("neg?") {
    def apply(a: LispNumber) = { if (a.negP) t else nil }
  })
  intern(new LispFn1[LispNumber]("sqrt") {
    def apply(a: LispNumber) = { new LispFlonum((math.sqrt(a.toJavaDouble))) }
  })
  intern(new LispFnP[LispObject]("eql?") {
    def apply(a: LispObject, b: LispObject) = eql(a, b)
  })
  intern(new LispFnP[LispNumber]("=") {
    def apply(a: LispNumber, b: LispNumber) = if (a == b) t else nil
  })
  intern(new LispFnP[LispChar]("char=") {
    def apply(a: LispChar, b: LispChar) = if (a.ch == b.ch) t else nil
  })
  intern(new LispFn2[LispArray, LispInteger]("aref") {
    def apply(a: LispArray, b: LispInteger) = a.aref(b.toJavaInt)
  })
  intern(new LispFn3[LispArray, LispInteger, LispObject]("aset") {
    def apply(a: LispArray, b: LispInteger, c: LispObject) = { a.aset(b.toJavaInt, c) }
  })
  intern(new LispFn0("time") {
    def apply() = new LispString(new Date().toString)
  })
  intern(new LispFn0("get-time") {
    def apply() = new LispFixnum(System.currentTimeMillis)
  })
  intern(new LispFn0("make-string-output-stream") {
    def apply() = new StringOutputStream
  })
  intern(new LispFn1[LispStream]("eof?") {
    def apply(a: LispStream) = if (a.eof) t else nil
  })
  intern(new LispFn("exit", 0, 1) {
    def apply(o: Args) = {
      System.exit(if (o.length < 1) 0 else (o(0).as[LispNumber]).toJavaInt)
      nil
    }
  })
  intern(new LispFn1[StringOutputStream]("get-output-stream-string") {
    def apply(a: StringOutputStream) = new LispString(a.getOutputStreamString)
  })
  intern(new LispFn("throw", 1, 2) {
    def apply(o: Args) = {
      if (o.length == 2) {
        if (o(1).isInstanceOf[LispString]) throw new LispException(o(0).as[Symbol], (o(1).as[LispString]).toJavaString)
        else if (o(1).isInstanceOf[JavaObject]) throw new LispException(o(0).as[Symbol], (o(1).as[JavaObject]).getObj.asInstanceOf[Throwable])
        else throw new LispException(Symbol.internalError, "Throw threw a throw.")
      }
      if (o(0).isInstanceOf[JavaObject] && (o(0).as[JavaObject]).getObj.isInstanceOf[LispException]) throw (o(0).as[JavaObject]).getObj.asInstanceOf[LispException]
      throw new LispException(o(0).as[Symbol])
    }
  })
  intern(new LispFn("make-array", 1) {
    def apply(o: Args) = {
      if (o(0).isInstanceOf[Cell]) new LispArray(o(0).as[Cell])
      else if (o(0).isInstanceOf[LispInteger]) new LispArray((o(0).as[LispInteger]).toJavaInt)
      else throw new LispException(Symbol.internalError, "make-array wants an integer or a list")
    }
  })
  intern(new LispFn("make-string", 2) {
    def apply(o: Args) = {
      new LispString((o(0).as[LispInteger]).toJavaInt, o(1).as[LispChar])
    }
  })
  intern(new LispFn("length", 1) {
    def apply(o: Args) = {
      new LispFixnum(
        if ((o(0) eq nil))
          0
        else if (o(0).isInstanceOf[Cell])
          (o(0).as[Cell]).length
        else
          (o(0).as[LispArray]).length)
    }
  })
  intern(new LispFnP[LispObject]("equal?") {
    def apply(a: LispObject, b: LispObject) = { if (if (a eq nil) (b eq nil) else (a == b)) t else nil }
  })
  intern(new LispFn1[LispObject]("sxhash") {
    def apply(a: LispObject) = new LispFixnum(if (a eq nil) 0 else a.hashCode)
  })
  intern(new LispFn("running-compiled?") {
    def apply(o: Args) = nil
  })
  intern(new LispFn1[LispChar]("char->integer") {
    def apply(a: LispChar) = new LispFixnum(a.ch.asInstanceOf[Int])
  })
  intern(new LispFn1[LispInteger]("integer->char") {
    def apply(a: LispInteger) = LispChar.create(a.toJavaInt.asInstanceOf[Char])
  })

  private val number: Symbol = intern("number")
  private val integer: Symbol = intern("integer")
  private val fixnum: Symbol = intern("fixnum")
  private val bignum: Symbol = intern("bignum")
  private val flonum: Symbol = intern("flonum")
  private val symbol: Symbol = intern("symbol")
  private val cons: Symbol = intern("cons")
  private val procedure: Symbol = intern("procedure")
  private val subr: Symbol = intern("subr")
  private val array: Symbol = intern("array")
  private val string: Symbol = intern("string")
  private val javaObject: Symbol = intern("java-object")
  private val javaMethod: Symbol = intern("java-method")
  //  private val exception: Symbol = intern("exception")
  private val charmander: Symbol = intern("char")
  private val stream: Symbol = intern("stream")
  private val list: Symbol = intern("list")

  intern(new LispFn2[Symbol, LispObject]("type?") {
    def apply(a: Symbol, b: LispObject) = {
      val knownType =
        if (a eq number) b.isInstanceOf[LispNumber]
        else if (a eq integer) b.isInstanceOf[LispInteger]
        else if (a eq fixnum) b.isInstanceOf[LispFixnum]
        else if (a eq bignum) b.isInstanceOf[LispBigInt]
        else if (a eq flonum) b.isInstanceOf[LispFlonum]
        else if (a eq symbol) b.isInstanceOf[Symbol]
        else if (a eq cons) b.isInstanceOf[Cell]
        else if (a eq list) ((b eq nil) || b.isInstanceOf[Cell])
        else if (a eq procedure) b.isInstanceOf[Procedure]
        else if (a eq subr) b.isInstanceOf[LispFn]
        else if (a eq array) b.isInstanceOf[LispArray]
        else if (a eq string) b.isInstanceOf[LispString]
        else if (a eq javaObject) b.isInstanceOf[JavaObject]
        else if (a eq javaMethod) b.isInstanceOf[JavaMethod]
        else if (a eq charmander) b.isInstanceOf[LispChar]
        else if (a eq stream) b.isInstanceOf[LispStream]
        else false
      if (knownType) t else nil
    }
  })
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