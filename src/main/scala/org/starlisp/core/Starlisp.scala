package org.starlisp.core

/*
** TODO: * Fix up exceptions in places to be nicer somehow. Especially
**         for read-related stuffs
**       * Think about lexical scoping... dynamic scoping might be more of a PITA than I thought initially (dynamic wins on
**         ease of implementation... _but_). Lexical might not need be so difficult given passable environments, also nlambdas
**         as a method for recursion would be sort of cute in this case (or do we use the y-combinator? =p)
**       * Think about a procedure abstract class or interface, for all things having something called "apply"
**       * Try later to move away from pure list structure for exprs, instead substituting with a subclass of Procedure
**         possibly internally containing the same list structure, this is going to make lexical scoping among other things
**         much smoother (as well as removing serious amounts of clutter from eval)
**       * Fix up EOF-handling
**       * Fix up equals for LispNumbers and more
*/

import java.io.FileReader
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import java.io.StringReader

object Starlisp {

  def toStringOrNull(obj: LispObject): String = Option(obj).getOrElse("nil").toString

  def prin1(obj: LispObject, stream: LispOutputStream): LispObject = {
    val s = if (stream != null) stream else Symbol.standardOutput.value.asInstanceOf[LispOutputStream]
    if (obj != null) { s.write(obj.toString) } else { s.write("nil") }
    obj
  }

  def read(stream: LispInputStream): LispObject = {
    (if (stream != null) stream else Symbol.standardInput.value).asInstanceOf[LispInputStream].read
  }

  def readChar(stream: LispInputStream): LispChar = {
    (if (stream != null) stream else Symbol.standardInput.value).asInstanceOf[LispInputStream].readChar
  }

  def writeChar(ch: LispChar, stream: LispOutputStream): LispChar = {
    (if (stream != null) stream else Symbol.standardOutput.value).asInstanceOf[LispOutputStream].write(ch.ch)
    ch
  }

  private def atom(obj: LispObject): LispObject = if (obj.isInstanceOf[Cell]) null else Symbol.t

  private def eq(obj1: LispObject, obj2: LispObject): LispObject = if (obj1 eq obj2) Symbol.t else null
  private def eql(a: LispObject, b: LispObject): LispObject = {
    if (a == null || b == null)
      eq(a, b)
    else if (!a.getClass.isInstance(b))
      null
    else if (a.isInstanceOf[LispChar])
      if (a.asInstanceOf[LispChar].ch == a.asInstanceOf[LispChar].ch) Symbol.t else null
    else if ((a.isInstanceOf[LispNumber]))
      if (a.asInstanceOf[LispNumber] == b.asInstanceOf[LispNumber]) Symbol.t else null
    else
      eq(a, b)
  }

  private def intern(str: String) = Symbol.intern(str)

  intern("nil").value = null
  intern("Class").value = new JavaObject(classOf[Class[_]])
  intern("cons").value = LispFn("cons") { o => new Cell(o(0), o(1)) }
  intern("car").value = LispFn1[Cell]("car") { o => if (o == null) null else o.car }
  intern("cdr").value = LispFn1[Cell]("cdr") { o => if (o == null) null else o.cdr }
  intern("rplaca").value = LispFn("rplaca", 2) { o =>
    (o(0).asInstanceOf[Cell]).setCar(o(1))
    o(0)
  }
  intern("rplacd").value = LispFn("rplacd", 2) { o =>
    (o(0).asInstanceOf[Cell]).setCdr(o(1).asInstanceOf[Cell])
    o(0)
  }
  intern("prin1").value = LispFn("prin1", 1, 2) { o =>
    prin1(o(0), if ((o.length > 1)) o(1).asInstanceOf[LispOutputStream] else null)
  }
  intern("eq?").value = LispFn2[LispObject]("eq?") { (a,b) => if (a eq b) Symbol.t else null }
  intern("atom?").value = LispFn1[LispObject]("atom?") { o => atom(o) }
  intern("set").value = LispFn2M[Symbol, LispObject]("set") { (a,b) => a.value = b; b }
  intern("symbol-value").value = LispFn1[Symbol]("symbol-value") { o => if (o == null) null else o.value }
  intern("intern").value = LispFn1[LispObject]("intern") { o =>
    if (o.isInstanceOf[LispString]) intern((o.asInstanceOf[LispString]).toJavaString)
    else if (o.isInstanceOf[Symbol]) Symbol.intern((o.asInstanceOf[Symbol]))
    else throw new LispException(Symbol.internalError, "Bad argument")
  }
  intern("+").value = LispFn2[LispNumber]("+") { (a,b) => a.add(b) }
  intern("-").value = LispFn2[LispNumber]("-") { (a,b) => a.sub(b) }
  intern("*").value = LispFn2[LispNumber]("*") { (a,b) => a.mul(b) }
  intern("/").value = LispFn2[LispNumber]("/") { (a,b) => a.div(b) }
  intern("mod").value = LispFn2[LispInteger]("mod") { (a,b) => a.mod(b) }
  intern("ash").value = LispFn2[LispInteger]("ash") { (a,b) => a.ash(b) }
  intern("neg?").value = LispFn1[LispNumber]("neg?") { o => if (o.negP) Symbol.t else null }
  intern("sqrt").value = LispFn1[LispNumber]("sqrt") { o => new LispFlonum((math.sqrt(o.toJavaDouble))) }
  intern("eql?").value = LispFn2[LispObject]("eql?") { (a,b) => eql(a, b) }
  intern("=").value = LispFn2[LispNumber]("=") { (a,b) => if (a == b) Symbol.t else null }
  intern("char=").value = LispFn2[LispChar]("char=") { (a,b) => if (a.ch == b.ch) Symbol.t else null }
  intern("aref").value = LispFn2M[LispArray, LispInteger]("aref") { (a,b) => a.aref(b.toJavaInt) }
  intern("aset").value = LispFn("aset", 3) { o =>
    (o(0).asInstanceOf[LispArray]).aset((o(1).asInstanceOf[LispInteger]).toJavaInt, o(2))
  }
  intern("system-exit").value = LispFn("exit", 0, 1) { o =>
    System.exit(if ((o.length < 1)) 0 else (o(0).asInstanceOf[LispNumber]).toJavaInt)
    null
  }
  intern("get-time").value = LispFn0("get-time") { () => new LispFixnum(System.currentTimeMillis) }
  intern("read-char").value = LispFn("read-char", 0, 1) { o =>
    try {
      readChar(if ((o.length > 0)) o(0).asInstanceOf[LispInputStream] else null)
    } catch {
      case e: IOException => {
        throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
      }
    }
  }
  intern("write-char").value = LispFn("write-char", 1, 2) { o =>
    try {
      writeChar(o(0).asInstanceOf[LispChar], (if ((o.length > 1)) o(1).asInstanceOf[LispOutputStream] else null))
    } catch {
      case e: IOException => {
        throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
      }
    }
  }
  intern("read").value = LispFn("read", 0, 1) { o =>
    try {
      read(if ((o.length > 0)) o(0).asInstanceOf[LispInputStream] else null)
    } catch {
      case e: IOException => {
        throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
      }
    }
  }
  intern("open").value = LispFn("open", 2) { o =>
    try {
      if (o(1) eq Symbol.in) new LispStreamImpl(new FileReader((o(0).asInstanceOf[LispString]).toJavaString), null)
      else if (o(1) eq Symbol.out) new LispStreamImpl(null, new PrintWriter(new FileWriter((o(0).asInstanceOf[LispString]).toJavaString)))
      else throw new LispException(Symbol.internalError, "You confused me, you want a stream out, or in?")
    } catch {
      case e: IOException => {
        throw new LispException(Symbol.internalError, e)
      }
    }
  }
  intern("close").value = LispFn1[LispStream]("close") { o =>
    try {
      if (o.close) Symbol.t else null
    } catch {
      case e: IOException => {
        throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
      }
    }
  }
  intern("eof?").value = LispFn1[LispStream]("eof?") { o => if (o.eof) Symbol.t else null }
  intern("make-string-input-stream").value = LispFn1[LispString]("make-string-input-stream") { o =>
    new LispStreamImpl(new StringReader(o.toJavaString), null)
  }
  intern("make-string-output-stream").value = LispFn0("make-string-output-stream") { () => new StringOutputStream }
  intern("get-output-stream-string").value = LispFn1[StringOutputStream]("get-output-stream-string") { o =>
    new LispString(o.getOutputStreamString)
  }
  intern("throw").value = LispFn("throw", 1, 2) { o =>
    if (o.length == 2) {
      if (o(1).isInstanceOf[LispString]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[LispString]).toJavaString)
      else if (o(1).isInstanceOf[JavaObject]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[JavaObject]).getObj.asInstanceOf[Throwable])
      else throw new LispException(Symbol.internalError, "Throw threw a throw.")
    }
    if (o(0).isInstanceOf[JavaObject] && (o(0).asInstanceOf[JavaObject]).getObj.isInstanceOf[LispException]) throw (o(0).asInstanceOf[JavaObject]).getObj.asInstanceOf[LispException]
    throw new LispException(o(0).asInstanceOf[Symbol])
  }
  intern("make-array").value = LispFn("make-array", 1) { o =>
    if (o(0).isInstanceOf[Cell]) new LispArray(o(0).asInstanceOf[Cell])
    else if (o(0).isInstanceOf[LispInteger]) new LispArray((o(0).asInstanceOf[LispInteger]).toJavaInt)
    else throw new LispException(Symbol.internalError, "make-array wants an integer or a list")
  }
  intern("make-string").value = LispFn("make-string", 2) { o =>
    new LispString((o(0).asInstanceOf[LispInteger]).toJavaInt, o(1).asInstanceOf[LispChar])
  }
  intern("length").value = LispFn("length", 1) { o =>
    new LispFixnum(if ((o(0) == null)) 0 else if ((o(0).isInstanceOf[Cell])) (o(0).asInstanceOf[Cell]).length else (o(0).asInstanceOf[LispArray]).length)
  }
  intern("equal?").value = LispFn2[LispObject]("equal?") { (a,b) =>
    if (if (a == null) (b == null) else (a == b)) Symbol.t else null
  }
  intern("sxhash").value = LispFn1[LispObject]("sxhash") { o => new LispFixnum(if (o == null) 0 else o.hashCode) }
  intern("running-compiled?").value = LispFn("running-compiled?") { o => null }
  intern("char->integer").value = LispFn("char->integer", 1) { o =>
    new LispFixnum((o(0).asInstanceOf[LispChar]).ch.asInstanceOf[Int])
  }
  intern("integer->char").value = LispFn1[LispInteger]("integer->char") { o =>
    LispChar.create(o.toJavaInt.asInstanceOf[Char])
  }
  intern("type?").value = LispFn2M[Symbol, LispObject]("type?") { (a,b) =>
    val knownType =
      if (a eq number) b.isInstanceOf[LispNumber]
      else if (a eq integer) b.isInstanceOf[LispInteger]
      else if (a eq fixnum) b.isInstanceOf[LispFixnum]
      else if (a eq bignum) b.isInstanceOf[LispBigInt]
      else if (a eq flonum) b.isInstanceOf[LispFlonum]
      else if (a eq symbol) b.isInstanceOf[Symbol]
      else if (a eq cons) b.isInstanceOf[Cell]
      else if (a eq list) (b == null || b.isInstanceOf[Cell])
      else if (a eq procedure) b.isInstanceOf[Procedure]
      else if (a eq subr) b.isInstanceOf[LispFn]
      else if (a eq array) b.isInstanceOf[LispArray]
      else if (a eq string) b.isInstanceOf[LispString]
      else if (a eq javaObject) b.isInstanceOf[JavaObject]
      else if (a eq javaMethod) b.isInstanceOf[JavaMethod]
      else if (a eq charmander) b.isInstanceOf[LispChar]
      else if (a eq stream) b.isInstanceOf[LispStream]
      else false
    if (knownType) Symbol.t else null
  }

  private[core] val number: Symbol = intern("number")
  private[core] val integer: Symbol = intern("integer")
  private[core] val fixnum: Symbol = intern("fixnum")
  private[core] val bignum: Symbol = intern("bignum")
  private[core] val flonum: Symbol = intern("flonum")
  private[core] val symbol: Symbol = intern("symbol")
  private[core] val cons: Symbol = intern("cons")
  private[core] val procedure: Symbol = intern("procedure")
  private[core] val subr: Symbol = intern("subr")
  private[core] val array: Symbol = intern("array")
  private[core] val string: Symbol = intern("string")
  private[core] val javaObject: Symbol = intern("java-object")
  private[core] val javaMethod: Symbol = intern("java-method")
  private[core] val exception: Symbol = intern("exception")
  private[core] val charmander: Symbol = intern("char")
  private[core] val stream: Symbol = intern("stream")
  private[core] val list: Symbol = intern("list")
}

class Runtime {

  import Starlisp._

  var done = false

  private val symbolContext = Symbol.cloneSymbols

  // Runtime specific stack and components
  private val DEFAULT_STACK_SIZE = 32768 * 2
  private var stackSize = 0
  private val stack = new Array[LispObject](DEFAULT_STACK_SIZE)
  private var genSymCounter = 0L

  private def intern(str: String) = symbolContext.intern(str)

  private def cons(car: LispObject, cdr: LispObject): Cell = new Cell(car, cdr)

  private final def saveEnvironment {
    stackSize += 1
  }

  private final def restoreEnvironment {
    stackSize -= 1
    while (stack(stackSize) != null) {
      (stack(stackSize).asInstanceOf[Symbol]).value = stack(stackSize - 1)
      stack(stackSize) = null
      stack(stackSize - 1) = null
      stackSize -= 2
    }
  }

  private final def bind(sbl: Symbol, value: LispObject) {
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

  private final def evlis(list: Cell): Cell = {
    if (list == null) return null
    var last = new Cell(evalHead(list.car), null)
    val result = last
    var c = list.cdr.asInstanceOf[Cell]
    while (c != null) {
      last = (last.setCdr(new Cell(evalHead(c.car), null))).asInstanceOf[Cell]
      c = c.cdr.asInstanceOf[Cell]
    }
    result
  }

  private final def evlisArray(list: Cell): Array[LispObject] = {
    val res: Array[LispObject] = new Array[LispObject](if ((list == null)) 0 else list.length)
    var i: Int = 0
    var c: Cell = list
    while (c != null) {
      res(i) = evalHead(c.car)
      i += 1
      c = c.cdr.asInstanceOf[Cell]
    }
    res
  }

  private final def evalHead(obj: LispObject): LispObject = {
    saveEnvironment
    try {
      evalTail(obj)
    } finally {
      restoreEnvironment
    }
  }

  /**
   * Evaluate code in the current dynamic environment
   */
  final def eval(obj: LispObject): LispObject = {
    evalHead(obj)
  }

  private final def evalTail(inobj: LispObject): LispObject = {
    var obj = inobj
    while (true) {
      if (obj.isInstanceOf[Symbol]) {
        return (obj.asInstanceOf[Symbol]).value
      } else if (obj.isInstanceOf[Cell]) {
        val list = obj.asInstanceOf[Cell]
        if (list.car eq Symbol._if) {
          val res = evalHead((list.cdr.asInstanceOf[Cell]).car)
          if (res != null) {
            obj = ((list.cdr.asInstanceOf[Cell]).cdr.asInstanceOf[Cell]).car
          } else if (((list.cdr.asInstanceOf[Cell]).cdr.asInstanceOf[Cell]).cdr != null) {
            obj = (((list.cdr.asInstanceOf[Cell]).cdr.asInstanceOf[Cell]).cdr.asInstanceOf[Cell]).car
          } else {
            return null
          }
        } else if (list.car eq Symbol.quote) {
          return (list.cdr.asInstanceOf[Cell]).car
        } else if ((list.car eq Symbol.lambda) || (list.car eq Symbol.`macro`)) {
          return list
        } else {
          val first = evalHead(list.car)
          if (first.isInstanceOf[Cell]) {
            val f1rst = first.asInstanceOf[Cell]
            if (f1rst.car eq Symbol.lambda) {
              val lambdaVar = (f1rst.cdr.asInstanceOf[Cell]).car
              var lambdaBody = (f1rst.cdr.asInstanceOf[Cell]).cdr.asInstanceOf[Cell]
              val argList = list.cdr.asInstanceOf[Cell]
              if (lambdaVar != null) {
                if (argList == null && lambdaVar.isInstanceOf[Cell]) throw new LispException(Symbol.internalError, "Too few args (zero in fact): " + obj)
                var evalledArgs = evlis(argList)
                if (lambdaVar.isInstanceOf[Symbol]) {
                  bind(lambdaVar.asInstanceOf[Symbol], evalledArgs)
                } else {
                  var c = lambdaVar.asInstanceOf[Cell]
                  var done = false
                  while (!done) {
                    if (c.cdr == null) {
                      if (evalledArgs.cdr != null) throw new LispException(Symbol.internalError, "Too many args: " + obj)
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      done = true
                    } else if (!(c.cdr.isInstanceOf[Cell])) {
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      bind(c.cdr.asInstanceOf[Symbol], evalledArgs.cdr)
                      done = true
                    } else {
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      evalledArgs = evalledArgs.cdr.asInstanceOf[Cell]
                      if (evalledArgs == null) throw new LispException(Symbol.internalError, "Too few args: " + obj)
                      c = c.cdr.asInstanceOf[Cell]
                    }
                  }
                }
              }
              if (lambdaBody == null) return null
              while (lambdaBody.cdr != null) {
                evalHead(lambdaBody.car)
                lambdaBody = lambdaBody.cdr.asInstanceOf[Cell]
              }
              obj = lambdaBody.car
            } else if (f1rst.car eq Symbol.`macro`) {
              obj = evalHead(cons(cons(Symbol.lambda, f1rst.cdr.asInstanceOf[Cell]), cons(cons(Symbol.quote, cons(list, null)), null)))
            } else {
              throw new LispException(Symbol.internalError, "You can't just pretend lists to be functions, when they aren't: " + obj.toString)
            }
          } else if (first.isInstanceOf[Procedure]) {
            return (first.asInstanceOf[Procedure]).applyArgs(evlisArray(list.cdr.asInstanceOf[Cell]))
          } else {
            throw new LispException(
              Symbol.internalError,
              "EVAL: %s is not a function name; try using a symbol instead. EXPR: %s".format(
                String.valueOf(first),
                Starlisp.toStringOrNull(obj)))
          }
        }
      } else {
        return obj
      }
    }
    obj
  }

  def prin1(obj: LispObject, stream: LispStream): LispObject = {
    val s = if ((stream != null)) stream else Symbol.standardOutput.value
    if (obj != null) {
      s.asInstanceOf[LispOutputStream].write(obj.toString)
    } else {
      s.asInstanceOf[LispOutputStream].write("nil")
    }
    obj
  }

  private def gensym: LispObject = {
    val id = "G%d".format(genSymCounter)
    genSymCounter += 1
    new Symbol(id)
  }

  // Initialize the Runtime-specific methods
  intern("eval").value = LispFn1[LispObject]("eval") { o => eval(o) }
  intern("symbols").value = LispFn("symbols") { o => symbolContext.getSymbols }
  intern("gensym").value = LispFn("gensym") { o => gensym }
  intern("make-runnable").value = LispFn("make-runnable", 1) { o =>
    new JavaObject(new Runnable {
      def run {
        eval(cons(o(0), null))
      }
    })
  }
  intern("%try").value = LispFn2[LispObject]("%try") { (a,b) =>
    try {
      eval(cons(a, null))
    }
    catch {
      case e: Exception => {
        eval(cons(b, cons(new JavaObject(e), null)))
      }
    }
  }
  intern("exit").value = LispFn("exit", 0, 1) { o =>
    done = true
    null
  }
}