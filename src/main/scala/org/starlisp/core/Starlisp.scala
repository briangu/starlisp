package org.starlisp.core

import java.io._
import scala.Predef._
import scala.Some

object Starlisp {

  val nil = null
  val t = Symbol.t

  type Args = Array[LispObject]

  def toStringOrNil(obj: LispObject): String = Option(obj).getOrElse("nil").toString

  def prin1(obj: LispObject, stream: LispOutputStream): LispObject = {
    val s = if (stream != nil) stream else Symbol.standardOutput.value.as[LispOutputStream]
    if (obj != nil) {s.write(obj.toString)} else {s.write("nil")}
    obj
  }

  def writeChar(ch: LispChar, stream: LispOutputStream): LispChar = {
    (if (stream != nil) stream else Symbol.standardOutput.value).as[LispOutputStream].write(ch.ch)
    ch
  }

  private def eq(obj1: LispObject, obj2: LispObject) = if (obj1 eq obj2) t else nil

  private def eql(a: LispObject, b: LispObject): LispObject = {
    if (a == nil || b == nil)
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

  private def intern(name: String) = Environment.root.intern(name)
  private def intern(proc: Procedure): Unit = intern(proc.name).value = proc

  intern("nil").value = nil
  intern("Class").value = new JavaObject(classOf[Class[_]])

  intern(new LispFn2("cons") { def apply(o: Args) = { new Cell(a(o), b(o)) } })
  intern(new LispFn1[Cell]("car") { def apply(o: Args) = { if (a(o) == nil) nil else a(o).car } })
  intern(new LispFn1[Cell]("cdr") { def apply(o: Args) = { if (a(o) == nil) nil else a(o).cdr } })
  intern(new Procedure("set") {
    def apply(env: Environment, o: Args) = { env(o(0).as[Symbol]).value = o(1); o(1) }
  })
  intern(new LispFn2[Cell, LispObject]("rplaca", 2) { def apply(o: Args) = { a(o).Car(b(o)); a(o) } })
  intern(new LispFn2[Cell, LispObject]("rplacd", 2) { def apply(o: Args) = { a(o).Cdr(b(o)); a(o) } })
  intern(new LispFn("prin1", 1, 2) {
    def apply(o: Args) = prin1(o(0), if ((o.length > 1)) o(1).as[LispOutputStream] else nil)
  })
  intern(new LispFn1[Symbol]("symbol-value") {
    def apply(o: Args) = { if (a(o) == nil) nil else a(o).value }
  })
  intern(new LispFn1[LispObject]("intern") {
    def apply(o: Args) = {
      if (a(o).isInstanceOf[LispString]) intern((a(o).as[LispString]).toJavaString)
      else if (a(o).isInstanceOf[Symbol]) Symbol.intern(a(o).as[Symbol])
      else throw new LispException(Symbol.internalError, "Bad argument")
    }
  })
  intern(new LispFnP[LispObject]("eq?") { def apply(o: Args) = { if (a(o) eq b(o)) t else nil } })
  intern(new LispFn1[LispObject]("atom?") { def apply(o: Args) = if (a(o).isInstanceOf[Cell]) nil else t })
  intern(new LispFnP[LispNumber]("+") { def apply(o: Args) = { a(o).add(b(o)) } })
  intern(new LispFnP[LispNumber]("-") { def apply(o: Args) = { a(o).sub(b(o)) } })
  intern(new LispFnP[LispNumber]("*") { def apply(o: Args) = { a(o).mul(b(o)) } })
  intern(new LispFnP[LispNumber]("/") { def apply(o: Args) = { a(o).div(b(o)) } })
  intern(new LispFnP[LispInteger]("mod") { def apply(o: Args) = { a(o).mod(b(o)) } })
  intern(new LispFnP[LispInteger]("ash") { def apply(o: Args) = { a(o).ash(b(o)) } })
  intern(new LispFn1[LispNumber]("neg?") { def apply(o: Args) = { if (a(o).negP) t else nil } })
  intern(new LispFn1[LispNumber]("sqrt") { def apply(o: Args) = { new LispFlonum((math.sqrt(a(o).toJavaDouble))) } })
  intern(new LispFnP[LispObject]("eql?") { def apply(o: Args) = eql(a(o), b(o)) })
  intern(new LispFnP[LispNumber]("=") { def apply(o: Args) = if (a(o) == b(o)) t else nil })
  intern(new LispFnP[LispChar]("char=") { def apply(o: Args) = if (a(o).ch == b(o).ch) t else nil })
  intern(new LispFn2[LispArray, LispInteger]("aref") { def apply(o: Args) = a(o).aref(b(o).toJavaInt) })
  intern(new LispFn("aset", 3) {
    def apply(o: Args) = {
      (o(0).as[LispArray]).aset((o(1).as[LispInteger]).toJavaInt, o(2))
    }
  })
  intern(new LispFn("exit", 0, 1) {
    def apply(o: Args) = {
      System.exit(if (o.length < 1) 0 else (o(0).as[LispNumber]).toJavaInt)
      nil
    }
  })
  intern(new LispFn("get-time") { def apply(o: Args) = new LispFixnum(System.currentTimeMillis) })
  intern(new LispFn("write-char", 1, 2) {
    def apply(o: Args) = {
      try {
        writeChar(o(0).as[LispChar], (if (o.length > 1) o(1).as[LispOutputStream] else nil))
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn("open", 2) {
    def apply(o: Args) = {
      try {
        if (o(1) eq Symbol.in) new LispStreamImpl(new FileReader((o(0).as[LispString]).toJavaString), nil)
        else if (o(1) eq Symbol.out) new LispStreamImpl(nil, new PrintWriter(new FileWriter((o(0).as[LispString]).toJavaString)))
        else throw new LispException(Symbol.internalError, "You confused me, you want a stream out, or in?")
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, e)
        }
      }
    }
  })
  intern(new LispFn1[LispStream]("close") {
    def apply(o: Args) = {
      try {
        if (a(o).close) t else nil
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn("make-string-output-stream") { def apply(o: Args) = new StringOutputStream })
  intern(new LispFn1[LispStream]("eof?") { def apply(o: Args) = if (a(o).eof) t else nil })
  intern(new LispFn1[LispString]("make-string-input-stream") {
    def apply(o: Args) = new LispStreamImpl(new StringReader(a(o).toJavaString), nil)
  })
  intern(new LispFn1[StringOutputStream]("get-output-stream-string") {
    def apply(o: Args) = new LispString(a(o).getOutputStreamString)
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
        if ((o(0) == nil))
          0
        else if (o(0).isInstanceOf[Cell])
          (o(0).as[Cell]).length
        else
          (o(0).as[LispArray]).length)
    }
  })
  intern(new LispFnP[LispObject]("equal?") {
    def apply(o: Args) = {
      if (if (a(o) == nil) (b(o) == nil) else (a(o) == b(o))) t else nil
    }
  })
  intern(new LispFn1[LispObject]("sxhash") {
    def apply(o: Args) = new LispFixnum(if (o == nil) 0 else o.hashCode)
  })
  intern(new LispFn("running-compiled?") {
    def apply(o: Args) = nil
  })
  intern(new LispFn1[LispChar]("char->integer", 1) {
    def apply(o: Args) = new LispFixnum(a(o).ch.asInstanceOf[Int])
  })
  intern(new LispFn1[LispInteger]("integer->char") {
    def apply(o: Args) = LispChar.create(a(o).toJavaInt.asInstanceOf[Char])
  })
  intern(new LispFn2[Symbol, LispObject]("type?") {
    def apply(o: Args) = {
      val knownType =
        if (a(o) eq number) b(o).isInstanceOf[LispNumber]
        else if (a(o) eq integer) b(o).isInstanceOf[LispInteger]
        else if (a(o) eq fixnum) b(o).isInstanceOf[LispFixnum]
        else if (a(o) eq bignum) b(o).isInstanceOf[LispBigInt]
        else if (a(o) eq flonum) b(o).isInstanceOf[LispFlonum]
        else if (a(o) eq symbol) b(o).isInstanceOf[Symbol]
        else if (a(o) eq cons) b(o).isInstanceOf[Cell]
        else if (a(o) eq list) (b(o) == nil || b(o).isInstanceOf[Cell])
        else if (a(o) eq procedure) b(o).isInstanceOf[Procedure]
        else if (a(o) eq subr) b(o).isInstanceOf[LispFn]
        else if (a(o) eq array) b(o).isInstanceOf[LispArray]
        else if (a(o) eq string) b(o).isInstanceOf[LispString]
        else if (a(o) eq javaObject) b(o).isInstanceOf[JavaObject]
        else if (a(o) eq javaMethod) b(o).isInstanceOf[JavaMethod]
        else if (a(o) eq charmander) b(o).isInstanceOf[LispChar]
        else if (a(o) eq stream) b(o).isInstanceOf[LispStream]
        else false
      if (knownType) t else nil
    }
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
  private val exception: Symbol = intern("exception")
  private val charmander: Symbol = intern("char")
  private val stream: Symbol = intern("stream")
  private val list: Symbol = intern("list")
}

class Runtime {

  import Starlisp._

  var stopped = false

  private val globalEnv = Environment.root.chain

  private def cons(car: LispObject, cdr: LispObject): Cell = new Cell(car, cdr)

  private def evlis(list: Cell, env: Environment): Cell = {
    if (list == null) return null
    var last = new Cell(eval(list.car, env), null)
    val result = last
    var c = list.Cdr[Cell]
    while (c != null) {
      last = (last.Cdr(new Cell(eval(c.car, env), null))).as[Cell]
      c = c.Cdr[Cell]
    }
    result
  }

  private def evlisArray(list: Cell, env: Environment): Args = {
    val res: Args = new Args(if ((list == null)) 0 else list.length)
    var i = 0
    var c = list
    while (c != null) {
      res(i) = eval(c.car, env)
      i += 1
      c = c.Cdr[Cell]
    }
    res
  }

  // TODO: remove inobj and make recursive
  def eval(inobj: LispObject, env: Environment = globalEnv): LispObject = {
    var obj = inobj
    while (true) {
      obj match {
        case symbol: Symbol => return env(symbol).value
        case list: Cell => {
          if (list.car eq Symbol._if) {
            Option(eval((list.Cdr[Cell]).car, env)) match {
              case Some(_) => obj = list.Cdr[Cell].Cdr[Cell].car
              case None => Option(list.Cdr[Cell].Cdr[Cell].Cdr[Cell]) match {
                case Some(cell) => obj = cell.car
                case None => return null
              }
            }
          } else if (list.car eq Symbol.quote) {
            return list.Cdr[Cell].car
          } else if ((list.car eq Symbol.lambda) || (list.car eq Symbol.`macro`)) {
            return list
          } else {
            eval(list.car, env) match {
              case first: Cell => {
                if (first.car eq Symbol.lambda) {
                  val closure = env.chain()
                  var lambdaBody = first.Cdr[Cell].Cdr[Cell]
                  if (lambdaBody == null) return null
                  val lambdaVar = first.Cdr[Cell].car
                  (Option(lambdaVar), Option(evlis(list.Cdr[Cell], closure))) match {
                    case (Some(symbol: Symbol), Some(args: Cell)) => closure.bind(symbol, args)
                    case (Some(head: Cell), Some(args: Cell)) => {
                      var cell = head
                      var evalledArgs = args
                      var done = false
                      while (!done) {
                        if (cell.cdr == null) {
                          if (evalledArgs.cdr != null)
                            throw new LispException(Symbol.internalError, "Too many args: " + obj)
                          closure.bind(cell.Car[Symbol], evalledArgs.car)
                          done = true
                        } else if (!(cell.cdr.isInstanceOf[Cell])) {
                          closure.bind(cell.Car[Symbol], evalledArgs.car)
                          closure.bind(cell.Cdr[Symbol], evalledArgs.cdr)
                          done = true
                        } else {
                          closure.bind(cell.Car[Symbol], evalledArgs.car)
                          evalledArgs = evalledArgs.Cdr[Cell]
                          if (evalledArgs == null)
                            throw new LispException(Symbol.internalError, "Too few args: " + obj)
                          cell = cell.Cdr[Cell]
                        }
                      }
                    }
                    case (Some(cell: Cell), None) =>
                      throw new LispException(Symbol.internalError, "Too few args (zero in fact): " + obj)
                    case (_, _) => ;
                  }
                  while (lambdaBody.cdr != null) {
                    eval(lambdaBody.car, closure)
                    lambdaBody = lambdaBody.Cdr[Cell]
                  }
                  obj = lambdaBody.car
                } else if (first.car eq Symbol.`macro`) {
                  obj = eval(cons(cons(Symbol.lambda, first.Cdr[Cell]), cons(cons(Symbol.quote, cons(list, null)), null)), env)
                } else {
                  throw new LispException(Symbol.internalError, "You can't just pretend lists to be functions, when they aren't: " + obj.toString)
                }
              }
              case proc: Procedure => {
                val args = evlisArray(list.Cdr[Cell], env)
                if (args.length < proc.minArgs) {
                  throw new LispException(Symbol.internalError, "Too few args when calling procedure: " + proc.toString)
                }
                if (args.length > proc.maxArgs) {
                  throw new LispException(Symbol.internalError, "Too many args when calling procedure: " + proc.toString)
                }
                return proc(env, args)
              }
              case unknown => {
                throw new LispException(
                  Symbol.internalError,
                  "EVAL: %s is not a function name; try using a symbol instead. EXPR: %s".format(
                    String.valueOf(unknown),
                    Starlisp.toStringOrNil(obj)))
              }
            }
          }
        }
        case _ => return obj
      }
    }
    obj
  }

  // Initialize the Runtime-specific methods
  private def intern(proc: Procedure) = globalEnv.intern(proc.name).value = proc

  val standardInput = globalEnv.intern("*standard-input*", new LispTokenizer(globalEnv, System.in, null))

  def read(stream: LispInputStream): LispObject = {
    (if (stream != nil) stream else standardInput.value).as[LispInputStream].read
  }

  def readChar(stream: LispInputStream): LispChar = {
    (if (stream != nil) stream else standardInput.value).as[LispInputStream].readChar
  }

  intern(new LispFn("read", 0, 1) {
    def apply(o: Args) = {
      try {
        read(if (o.length > 0) o(0).as[LispInputStream] else nil)
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
        }
      }
    }
  })

  intern(new LispFn("read-char", 0, 1) {
    def apply(o: Args) = {
      try {
        readChar(if (o.length > 0) o(0).as[LispInputStream] else nil)
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
        }
      }
    }
  })

  intern(new LispFn1[LispObject]("eval") {def apply(o: Args) = eval(o(0), globalEnv)})
  intern(new LispFn("symbols") {def apply(o: Args) = globalEnv.getSymbols})
  intern(new LispFn("gensym") {def apply(o: Args) = globalEnv.gensym})
  intern(new LispFn("make-runnable", 1) {
    def apply(o: Args) = {
      new JavaObject(new Runnable {
        def run { eval(cons(o(0), null), globalEnv) }
      })
    }
  })
  intern(new LispFnP[LispObject]("%try") {
    def apply(o: Args) = {
      try {
        eval(cons(a(o), null), globalEnv)
      }
      catch {
        case e: Exception => {
          eval(cons(b(o), cons(new JavaObject(e), null)), globalEnv)
        }
      }
    }
  })
  intern(new LispFn("exit", 0, 1) {
    def apply(o: Args) = {
      stopped = true
      null
    }
  })
}