package org.starlisp.core

import java.io._
import scala.Predef._
import scala.Some

class Runtime {

  val nil = null
  val t = Symbol.t
  type Args = Array[LispObject]

  var stopped = false

  private val globalEnv: Environment = RootEnvironment.chain

  private def error(msg: String): LispObject = {
    throw new LispException(Symbol.internalError, msg)
    nil
  }

  private def cons(car: LispObject, cdr: LispObject = null): Cell = new Cell(car, cdr)

  private def evlis(list: Cell, env: Environment): Cell = {
    if (list == null) return null
    var last = new Cell(eval(list.car, env))
    val result = last
    var c = list.rest
    while (c != null) {
      last = (last.Cdr(new Cell(eval(c.car, env)))).as[Cell]
      c = c.rest
    }
    result
  }

  def eval(obj: LispObject, env: Environment = globalEnv): LispObject = {
    obj match {
      case symbol: Symbol => {
        if (symbol.value eq null) {
          val sym = env.find(symbol)
          if (sym eq None)
            error("The variable %s is unbound.".format(symbol.name))
          else
            sym.get.value
        } else {
          symbol.value
        }
      }
      case list: Cell => {
        eval(list.car, env) match {
          case first: Cell => {
            first.car match {
              case fn: Symbol => {
                if (fn.name == Symbol.lambda.name) {
                  evalLambda(list.rest, first.rest, env.chain)
                } else if (fn.name == Symbol.`macro`.name) {
                  evalmacro(list, first.rest, env)
                } else {
                  error("%s is not a function.".format(list.car.toString))
                }
              }
              case _ => error("unknown first: %s".format(list.car))
            }
          }
          case proc: Procedure => proc(env, list, eval)
          case unknown => {
            error("EVAL: %s is not a function name; try using a symbol instead. EXPR: %s".format(
                  String.valueOf(list.car),
                  LispObject.toStringOrNil(obj)))
          }
        }
      }
      case _ => obj
    }
  }

  private def evalmacro(list: Cell, second: Cell, env: Environment): LispObject = {
    eval(eval(cons(cons(Symbol.lambda, second), cons(cons(Symbol.quote, cons(list)))), env), env)
  }

  private def pairlis(cell: Cell, argsList: Cell, env: Environment) {
    var vars = cell
    var args = argsList
    var done = false
    while (!done) {
      if (vars.cdr == null) {
        if (args.cdr != null) error("Too many args: " + argsList)
        env.bind(vars.Car[Symbol], args.car)
        done = true
      } else if (!(vars.cdr.isInstanceOf[Cell])) {
        env.bind(vars.Car[Symbol], args.car)
        env.bind(vars.Cdr[Symbol], args.cdr)
        done = true
      } else {
        env.bind(vars.Car[Symbol], args.car)
        args = args.rest
        if (argsList == null) error("Too few args: " + argsList)
        vars = vars.rest
      }
    }
  }

  private def evalLambda(args: Cell, second: Cell, env: Environment): LispObject = {
    var lambdaBody = second.rest
    if (lambdaBody == null) return null // TODO: fix?
    val lambdaVar = second.car
    (Option(lambdaVar), Option(evlis(args, env))) match {
      case (Some(symbol: Symbol), Some(argsList: Cell)) => env.bind(symbol, argsList)
      case (Some(head: Cell), Some(argsList: Cell)) => pairlis(head, argsList, env)
      case (Some(cell: Cell), None) => error("Too few args (zero in fact): " + args)
      case (_, _) => ;
    }
    while (lambdaBody.cdr != null) {
      eval(lambdaBody.car, env)
      lambdaBody = lambdaBody.rest
    }
    eval(lambdaBody.car, env)
  }

  // Initialize the Runtime-specific methods
  private def intern(proc: Procedure) {
    globalEnv.intern(proc.name).value = proc
  }

  val standardInput = globalEnv.intern("*standard-input*", new LispInputStreamReader(globalEnv, System.in))

  def read(stream: LispInputStream): LispObject = {
    Option(stream).getOrElse(standardInput.value).as[LispInputStream].read
  }

  def readChar(stream: LispInputStream): LispChar = {
    Option(stream).getOrElse(standardInput.value).as[LispInputStream].readChar
  }

  def prin1(obj: LispObject, stream: LispOutputStream): LispObject = {
    val s = if (stream != nil) stream else Symbol.standardOutput.value.as[LispOutputStream]
    if (obj != nil) {s.write(obj.toString)} else {s.write("nil")}
    obj
  }
  def writeChar(ch: LispChar, stream: LispOutputStream): LispChar = {
    (if (stream != nil) stream else Symbol.standardOutput.value).as[LispOutputStream].write(ch.ch)
    ch
  }

  intern(new LispFn("prin1", 1, 2) {
    def apply(o: Args) = prin1(o(0), if ((o.length > 1)) o(1).as[LispOutputStream] else nil)
  })
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
  intern(new LispFn1[LispStream]("close") {
    def apply(a: LispStream) = {
      try {
        if (a.close) t else nil
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, "An IOException just ocurred to me, " + this.toString)
        }
      }
    }
  })
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
  intern(new Procedure("set") {
    def apply(env: Environment, head: Cell, eval: (LispObject, Environment) => LispObject) = {
      val list = head.rest
      if (list eq null) error("Too few args when calling procedure: " + toString)
      val symRef = eval(list.car, env)
      if (!symRef.isInstanceOf[Symbol]) {
        if (list.car.isInstanceOf[Symbol]) {
          error("%s is not bound to a symbol.".format(list.Car[Symbol].name))
        } else {
          error("%s does not reference a symbol.".format(list.car.toString))
        }
      }
      val sym = symRef.as[Symbol]
      if (list.rest eq null) error("Too few args when calling procedure: " + toString)
      val b = eval(list.rest.car, env)

      env.find(sym) match {
        case Some(foundSymbol) => {
          // TODO: should we also set the value of sym ?
          foundSymbol.value = b
        }
        case None => {
          sym.value = b
          globalEnv.intern(sym) // TODO: hack! we are setting the globalEnv on miss so setq works
        }
      }
      b
    }
  })
  intern(new Procedure("open", 2) {
    def apply(env: Environment, head: Cell, eval: (LispObject, Environment) => LispObject) = {
      val list = head.rest
      try {
        if (list eq null) error("Too few args when calling procedure: " + toString)
        val a = eval(list.car, env).as[LispString].toJavaString
        if (list.rest eq null) error("Too few args when calling procedure: " + toString)
        val b = eval(list.rest.car, env)

        if (b eq Symbol.in) new LispInputStreamReader(env, new FileReader(a))
        else if (b eq Symbol.out) new LispOutputStreamWriter(new PrintWriter(new FileWriter(a)))
        else throw new LispException(Symbol.internalError, "You confused me, you want a stream out, or in?")
      } catch {
        case e: IOException => {
          throw new LispException(Symbol.internalError, e)
        }
      }
    }
  })
  intern(new Procedure("make-string-input-stream") {
    def apply(env: Environment, head: Cell, eval: (LispObject, Environment) => LispObject) = {
      val list = head.rest
      if (list eq null) error("Too few args when calling procedure: " + toString)
      val a = eval(list.car, env).as[LispString].toJavaString
      new LispInputStreamReader(env, new StringReader(a))
    }
  })
  intern(new LispFn("symbols") {def apply(o: Args) = globalEnv.getSymbols})
  intern(new LispFn("make-runnable", 1) {
    def apply(o: Args) = {
      new JavaObject(new Runnable {
        def run() { eval(cons(o(0), null), globalEnv) }
      })
    }
  })
  intern(new LispFnP[LispObject]("%try") {
    def apply(a: LispObject, b: LispObject) = {
      try {
        eval(cons(a, null), globalEnv)
      }
      catch {
        case e: Exception => {
          eval(cons(b, cons(new JavaObject(e), null)), globalEnv)
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
