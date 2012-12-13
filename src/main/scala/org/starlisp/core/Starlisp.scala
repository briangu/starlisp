package org.starlisp.core

/*
** TODO: * Fix up exceptions in places to be nicer somehow. Especially
**         for read-related stuffs
**       * Think about lexical scoping... dynamic scoping might be more of a PITA than I thought initially (dynamic wins on
**         ease of implementation... _but_). Lexical might not need be so difficult given passable environments, also nlambdas
**         as a method for recursion would be sort of cute in this case (or do we use the y-combinator? =p)
**       * Try later to move away from pure list structure for exprs, instead substituting with a subclass of Procedure
**         possibly internally containing the same list structure, this is going to make lexical scoping among other things
**         much smoother (as well as removing serious amounts of clutter from eval)
**       * Fix up EOF-handling
*/

import java.io._

object Starlisp {

  def toStringOrNull(obj: LispObject): String = Option(obj).getOrElse("nil").toString

  def prin1(obj: LispObject, stream: LispOutputStream): LispObject = {
    val s = if (stream != null) stream else Symbol.standardOutput.value.asInstanceOf[LispOutputStream]
    if (obj != null) {s.write(obj.toString)} else {s.write("nil")}
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

  private def eq(obj1: LispObject, obj2: LispObject) = if (obj1 eq obj2) Symbol.t else null

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

  private def intern(name: String) = Environment.root.intern(name)
  private def intern(proc: Procedure): Unit = intern(proc.name).value = proc

  intern("nil").value = null
  intern("Class").value = new JavaObject(classOf[Class[_]])

  intern(new LispFn2("cons") { def apply(o: Array[LispObject]) = { new Cell(a(o), b(o)) } })
  intern(new LispFn1[Cell]("car") { def apply(o: Array[LispObject]) = { if (a(o) == null) null else a(o).car } })
  intern(new LispFn1[Cell]("cdr") { def apply(o: Array[LispObject]) = { if (a(o) == null) null else a(o).cdr } })
  intern(new Procedure("set") {
    def apply(env: Environment, o: Array[LispObject]) = { env.find(o(0).asInstanceOf[Symbol]).value = o(1); o(1) }
  })
  intern(new LispFn2[Cell, LispObject]("rplaca", 2) { def apply(o: Array[LispObject]) = { a(o).Car(b(o)); a(o) } })
  intern(new LispFn2[Cell, LispObject]("rplacd", 2) { def apply(o: Array[LispObject]) = { a(o).Cdr(b(o)); a(o) } })
  intern(new LispFn("prin1", 1, 2) {
    def apply(o: Array[LispObject]) = prin1(o(0), if ((o.length > 1)) o(1).asInstanceOf[LispOutputStream] else null)
  })
  intern(new LispFn2[LispObject, LispObject]("eq?") {
    def apply(o: Array[LispObject]) = { if (a(o) eq b(o)) Symbol.t else null }
  })
  intern(new LispFn1[LispObject]("atom?") {
    def apply(o: Array[LispObject]) = if (a(o).isInstanceOf[Cell]) null else Symbol.t
  })
  intern(new LispFn1[Symbol]("symbol-value") {
    def apply(o: Array[LispObject]) = { if (a(o) == null) null else a(o).value }
  })
  intern(new LispFn1[LispObject]("intern") {
    def apply(o: Array[LispObject]) = {
      if (a(o).isInstanceOf[LispString]) intern((a(o).asInstanceOf[LispString]).toJavaString)
      else if (a(o).isInstanceOf[Symbol]) Symbol.intern(a(o).asInstanceOf[Symbol])
      else throw new LispException(Symbol.internalError, "Bad argument")
    }
  })
  intern(new LispFn2[LispNumber, LispNumber]("+") { def apply(o: Array[LispObject]) = { a(o).add(b(o)) } })
  intern(new LispFn2[LispNumber, LispNumber]("-") { def apply(o: Array[LispObject]) = { a(o).sub(b(o)) } })
  intern(new LispFn2[LispNumber, LispNumber]("*") { def apply(o: Array[LispObject]) = { a(o).mul(b(o)) } })
  intern(new LispFn2[LispNumber, LispNumber]("/") { def apply(o: Array[LispObject]) = { a(o).div(b(o)) } })
  intern(new LispFn2[LispInteger, LispInteger]("mod") { def apply(o: Array[LispObject]) = { a(o).mod(b(o)) } })
  intern(new LispFn2[LispInteger, LispInteger]("ash") {
    def apply(o: Array[LispObject]) = { a(o).ash(b(o)) }
  })
  intern(new LispFn1[LispNumber]("neg?") {
    def apply(o: Array[LispObject]) = { if (a(o).negP) Symbol.t else null }
  })
  intern(new LispFn1[LispNumber]("sqrt") {
    def apply(o: Array[LispObject]) = { new LispFlonum((math.sqrt(a(o).toJavaDouble))) }
  })
  intern(new LispFn2[LispObject, LispObject]("eql?") {
    def apply(o: Array[LispObject]) = eql(a(o), b(o))
  })
  intern(new LispFn2[LispNumber, LispNumber]("=") {
    def apply(o: Array[LispObject]) = if (a(o) == b(o)) Symbol.t else null
  })
  intern(new LispFn2[LispChar, LispChar]("char=") {
    def apply(o: Array[LispObject]) = if (a(o).ch == b(o).ch) Symbol.t else null
  })
  intern(new LispFn2[LispArray, LispInteger]("aref") {
    def apply(o: Array[LispObject]) = a(o).aref(b(o).toJavaInt)
  })
  intern(new LispFn("aset", 3) {
    def apply(o: Array[LispObject]) = {
      (o(0).asInstanceOf[LispArray]).aset((o(1).asInstanceOf[LispInteger]).toJavaInt, o(2))
    }
  })
  intern(new LispFn("exit", 0, 1) {
    def apply(o: Array[LispObject]) = {
      System.exit(if (o.length < 1) 0 else (o(0).asInstanceOf[LispNumber]).toJavaInt)
      null
    }
  })
  intern(new LispFn("get-time") {
    def apply(o: Array[LispObject]) = new LispFixnum(System.currentTimeMillis)
  })
  intern(new LispFn("read-char", 0, 1) {
    def apply(o: Array[LispObject]) = {
      try {
        readChar(if (o.length > 0) o(0).asInstanceOf[LispInputStream] else null)
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn("write-char", 1, 2) {
    def apply(o: Array[LispObject]) = {
      try {
        writeChar(o(0).asInstanceOf[LispChar], (if (o.length > 1) o(1).asInstanceOf[LispOutputStream] else null))
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just occured to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn("read", 0, 1) {
    def apply(o: Array[LispObject]) = {
      try {
        read(if (o.length > 0) o(0).asInstanceOf[LispInputStream] else null)
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn("open", 2) {
    def apply(o: Array[LispObject]) = {
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
  })
  intern(new LispFn1[LispStream]("close") {
    def apply(o: Array[LispObject]) = {
      try {
        if (a(o).close) Symbol.t else null
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
        }
      }
    }
  })
  intern(new LispFn1[LispStream]("eof?") {
    def apply(o: Array[LispObject]) = if (a(o).eof) Symbol.t else null
  })
  intern(new LispFn1[LispString]("make-string-input-stream") {
    def apply(o: Array[LispObject]) = new LispStreamImpl(new StringReader(a(o).toJavaString), null)
  })
  intern(new LispFn("make-string-output-stream") {
    def apply(o: Array[LispObject]) = new StringOutputStream
  })
  intern(new LispFn1[StringOutputStream]("get-output-stream-string") {
    def apply(o: Array[LispObject]) = new LispString(a(o).getOutputStreamString)
  })
  intern(new LispFn("throw", 1, 2) {
    def apply(o: Array[LispObject]) = {
      if (o.length == 2) {
        if (o(1).isInstanceOf[LispString]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[LispString]).toJavaString)
        else if (o(1).isInstanceOf[JavaObject]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[JavaObject]).getObj.asInstanceOf[Throwable])
        else throw new LispException(Symbol.internalError, "Throw threw a throw.")
      }
      if (o(0).isInstanceOf[JavaObject] && (o(0).asInstanceOf[JavaObject]).getObj.isInstanceOf[LispException]) throw (o(0).asInstanceOf[JavaObject]).getObj.asInstanceOf[LispException]
      throw new LispException(o(0).asInstanceOf[Symbol])
    }
  })
  intern(new LispFn("make-array", 1) {
    def apply(o: Array[LispObject]) = {
      if (o(0).isInstanceOf[Cell]) new LispArray(o(0).asInstanceOf[Cell])
      else if (o(0).isInstanceOf[LispInteger]) new LispArray((o(0).asInstanceOf[LispInteger]).toJavaInt)
      else throw new LispException(Symbol.internalError, "make-array wants an integer or a list")
    }
  })
  intern(new LispFn("make-string", 2) {
    def apply(o: Array[LispObject]) = {
      new LispString((o(0).asInstanceOf[LispInteger]).toJavaInt, o(1).asInstanceOf[LispChar])
    }
  })
  intern(new LispFn("length", 1) {
    def apply(o: Array[LispObject]) = {
      new LispFixnum(
        if ((o(0) == null))
          0
        else if (o(0).isInstanceOf[Cell])
          (o(0).asInstanceOf[Cell]).length
        else
          (o(0).asInstanceOf[LispArray]).length)
    }
  })
  intern(new LispFn2[LispObject, LispObject]("equal?") {
    def apply(o: Array[LispObject]) = {
      if (if (a(o) == null) (b(o) == null) else (a(o) == b(o))) Symbol.t else null
    }
  })
  intern(new LispFn1[LispObject]("sxhash") {
    def apply(o: Array[LispObject]) = new LispFixnum(if (o == null) 0 else o.hashCode)
  })
  intern(new LispFn("running-compiled?") {
    def apply(o: Array[LispObject]) = null
  })
  intern(new LispFn1[LispChar]("char->integer", 1) {
    def apply(o: Array[LispObject]) = new LispFixnum(a(o).ch.asInstanceOf[Int])
  })
  intern(new LispFn1[LispInteger]("integer->char") {
    def apply(o: Array[LispObject]) = LispChar.create(a(o).toJavaInt.asInstanceOf[Char])
  })
  intern(new LispFn2[Symbol, LispObject]("type?") {
    def apply(o: Array[LispObject]) = {
      val knownType =
        if (a(o) eq number) b(o).isInstanceOf[LispNumber]
        else if (a(o) eq integer) b(o).isInstanceOf[LispInteger]
        else if (a(o) eq fixnum) b(o).isInstanceOf[LispFixnum]
        else if (a(o) eq bignum) b(o).isInstanceOf[LispBigInt]
        else if (a(o) eq flonum) b(o).isInstanceOf[LispFlonum]
        else if (a(o) eq symbol) b(o).isInstanceOf[Symbol]
        else if (a(o) eq cons) b(o).isInstanceOf[Cell]
        else if (a(o) eq list) (b(o) == null || b(o).isInstanceOf[Cell])
        else if (a(o) eq procedure) b(o).isInstanceOf[Procedure]
        else if (a(o) eq subr) b(o).isInstanceOf[LispFn]
        else if (a(o) eq array) b(o).isInstanceOf[LispArray]
        else if (a(o) eq string) b(o).isInstanceOf[LispString]
        else if (a(o) eq javaObject) b(o).isInstanceOf[JavaObject]
        else if (a(o) eq javaMethod) b(o).isInstanceOf[JavaMethod]
        else if (a(o) eq charmander) b(o).isInstanceOf[LispChar]
        else if (a(o) eq stream) b(o).isInstanceOf[LispStream]
        else false
      if (knownType) Symbol.t else null
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

  var stopped = false

  private val globalEnv = Environment.root.chain

  private def cons(car: LispObject, cdr: LispObject): Cell = new Cell(car, cdr)

  private def evlis(list: Cell, env: Environment): Cell = {
    if (list == null) return null
    var last = new Cell(eval(list.car, env), null)
    val result = last
    var c = list.Cdr[Cell]
    while (c != null) {
      last = (last.Cdr(new Cell(eval(c.car, env), null))).asInstanceOf[Cell]
      c = c.Cdr[Cell]
    }
    result
  }

  private def evlisArray(list: Cell, env: Environment): Array[LispObject] = {
    val res: Array[LispObject] = new Array[LispObject](if ((list == null)) 0 else list.length)
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
        case symbol: Symbol => return env.find(symbol).value
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
                    Starlisp.toStringOrNull(obj)))
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

  intern(new LispFn1[LispObject]("eval") {def apply(o: Array[LispObject]) = eval(o(0), globalEnv)})
  intern(new LispFn("symbols") {def apply(o: Array[LispObject]) = globalEnv.getSymbols})
  intern(new LispFn("gensym") {def apply(o: Array[LispObject]) = globalEnv.gensym})
  intern(new LispFn("make-runnable", 1) {
    def apply(o: Array[LispObject]) = {
      new JavaObject(new Runnable {
        def run {
          eval(cons(o(0), null), globalEnv)
        }
      })
    }
  })
  intern(new LispFn2[LispObject, LispObject]("%try") {
    def apply(o: Array[LispObject]) = {
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
    def apply(o: Array[LispObject]) = {
      stopped = true
      null
    }
  })
}