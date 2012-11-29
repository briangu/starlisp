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
import java.io.UnsupportedEncodingException

object Starlisp {

  final val t: Symbol = intern("t")
  final val standardOutput = intern("*standard-output*")
  final val standardInput = intern("*standard-input*")
  final val standardError = intern("*standard-error*")
  final val lambda = intern("lambda")
  final val quote = intern("quote")
  final val _if= intern("if")
  final val `macro`= intern("macro")
  final val internalError= intern("internal-error")
  private final val in= intern("in")
  private final val out= intern("out")
  private final val STACK_SIZE: Int = 32768 * 2
  private var stackSize: Int = 0
  private val stack: Array[LispObject] = new Array[LispObject](STACK_SIZE)
  private var genSymCounter: Long = 0

  def toStringOrNull(obj: LispObject): String = {
    if ((obj != null)) obj.toString else "nil"
  }

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

  private final def evlis(list: Cons): Cons = {
    var result: Cons = null
    var last: Cons = null
    if (list == null) return null
    result = ({
      last = new Cons(evalHead(list.car), null); last
    })
    var c: Cons = list.cdr.asInstanceOf[Cons]
    while (c != null) {
      last = (last.setCdr(new Cons(evalHead(c.car), null))).asInstanceOf[Cons]
      c = c.cdr.asInstanceOf[Cons]
    }
    result
  }

  private final def evlisArray(list: Cons): Array[LispObject] = {
    val res: Array[LispObject] = new Array[LispObject](if ((list == null)) 0 else list.length)
    var i: Int = 0
    var c: Cons = list
    while (c != null) {
      res(i) = evalHead(c.car)
      i += 1
      c = c.cdr.asInstanceOf[Cons]
    }
    res
  }

  private final def evalHead(obj: LispObject): LispObject = {
    var res: LispObject = null
    saveEnvironment
    try {
      res = evalTail(obj)
    }
    finally {
      restoreEnvironment
    }
    res
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
      if (obj.isInstanceOf[Symbol]) return (obj.asInstanceOf[Symbol]).value
      else if (obj.isInstanceOf[Cons]) {
        val list: Cons = obj.asInstanceOf[Cons]
        if (list.car eq _if) {
          val res: LispObject = evalHead((list.cdr.asInstanceOf[Cons]).car)
          if (res != null) {
            obj = ((list.cdr.asInstanceOf[Cons]).cdr.asInstanceOf[Cons]).car
            //continue //todo: continue is not supported
          }
          else if (((list.cdr.asInstanceOf[Cons]).cdr.asInstanceOf[Cons]).cdr != null) {
            obj = (((list.cdr.asInstanceOf[Cons]).cdr.asInstanceOf[Cons]).cdr.asInstanceOf[Cons]).car
            //continue //todo: continue is not supported
          }
          else return null
        }
        else if (list.car == quote) return (list.cdr.asInstanceOf[Cons]).car
        else if (list.car == lambda || list.car == `macro`) return list
        else {
          val first: LispObject = evalHead(list.car)
          if (first.isInstanceOf[Cons]) {
            val f1rst: Cons = first.asInstanceOf[Cons]
            if (f1rst.car eq lambda) {
              val lambdaVar: LispObject = (f1rst.cdr.asInstanceOf[Cons]).car
              var lambdaBody: Cons = (f1rst.cdr.asInstanceOf[Cons]).cdr.asInstanceOf[Cons]
              val argList: Cons = list.cdr.asInstanceOf[Cons]
              if (lambdaVar != null) {
                if (argList == null && lambdaVar.isInstanceOf[Cons]) throw new LispException(internalError, "Too few args (zero in fact): " + obj)
                var evalledArgs: Cons = evlis(argList)
                if (lambdaVar.isInstanceOf[Symbol]) bind(lambdaVar.asInstanceOf[Symbol], evalledArgs)
                else {
                  var c: Cons = lambdaVar.asInstanceOf[Cons]
                  var done = false
                  while (!done) {
                    if (c.cdr == null) {
                      if (evalledArgs.cdr != null) throw new LispException(internalError, "Too many args: " + obj)
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      done = true
                    } else if (!(c.cdr.isInstanceOf[Cons])) {
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      bind(c.cdr.asInstanceOf[Symbol], evalledArgs.cdr)
                      done = true
                    } else {
                      bind(c.car.asInstanceOf[Symbol], evalledArgs.car)
                      evalledArgs = evalledArgs.cdr.asInstanceOf[Cons]
                      if (evalledArgs == null) throw new LispException(internalError, "Too few args: " + obj)
                      c = c.cdr.asInstanceOf[Cons]
                    }
                  }
                }
              }
              if (lambdaBody == null) return null
              while (lambdaBody.cdr != null) {
                evalHead(lambdaBody.car)
                lambdaBody = lambdaBody.cdr.asInstanceOf[Cons]
              }
              obj = lambdaBody.car
              //continue //todo: continue is not supported
            }
            else if (f1rst.car eq `macro`) {
              obj = evalHead(cons(cons(lambda, f1rst.cdr.asInstanceOf[Cons]), cons(cons(quote, cons(list, null)), null)))
              //continue //todo: continue is not supported
            }
            else throw new LispException(internalError, "You can't just pretend lists to be functions, when they aren't: " + obj.toString)
          }
          else if (first.isInstanceOf[Procedure]) return (first.asInstanceOf[Procedure]).applyArgs(evlisArray(list.cdr.asInstanceOf[Cons]))
          else throw new LispException(internalError, "internal error: " + toStringOrNull(obj))
        }
      }
      else return obj
    }
    obj // TODO: correct?
  }

  def prin1(obj: LispObject, stream: LispStream): LispObject = {
    val s = if ((stream != null)) stream else standardOutput.value
    if (obj != null) {
      s.asInstanceOf[LispStream].writeJavaString(obj.toString)
    } else {
      s.asInstanceOf[LispStream].writeJavaString("nil")
    }
    obj
  }

  def cons(car: LispObject, cdr: LispObject): Cons = {
    new Cons(car, cdr)
  }

  def car(list: Cons): LispObject = {
    if ((list == null)) null else list.car
  }

  def cdr(list: Cons): LispObject = {
    if ((list == null)) null else list.cdr
  }

  def intern(str: String): Symbol = {
    Symbol.intern(str)
  }

  def read(stream: LispStream): LispObject = {
    (if ((stream != null)) stream else standardInput.value).asInstanceOf[LispStream].read
  }

  def readChar(stream: LispStream): LispChar = {
    new LispChar((if ((stream != null)) stream else standardInput.value).asInstanceOf[LispStream].readJavaChar)
  }

  def writeChar(ch: LispChar, stream: LispStream): LispChar = {
    (if (stream != null) stream else standardOutput.value).asInstanceOf[LispStream].writeJavaChar(ch.ch)
    ch
  }

  def eq(obj1: LispObject, obj2: LispObject): LispObject = {
    if (obj1 eq obj2) t else null
  }

  def symbols: Cons = {
    Symbol.getSymbols
  }

  def symbolValue(sbl: Symbol): LispObject = {
    sbl.value
  }

  def atom(obj: LispObject): LispObject = {
    if ((obj.isInstanceOf[Cons])) null else t
  }

  def gensym: LispObject = {
    val id = "G%d".format(genSymCounter)
    genSymCounter += 1
    new Symbol(id)
  }

  def eql(a: LispObject, b: LispObject): LispObject = {
    if ((a == null || b == null)) eq(a, b) else if (!a.getClass.isInstance(b)) null else if ((a.isInstanceOf[LispChar])) if (((a.asInstanceOf[LispChar]).ch == (a.asInstanceOf[LispChar]).ch)) t else null else if ((a.isInstanceOf[LispNumber])) (if (((a.asInstanceOf[LispNumber]) == b.asInstanceOf[LispNumber])) t else null) else eq(a, b)
  }

  def initEnvironment {
    t.value = t
    try {
      standardOutput.value = new LispStream(null, System.out)
      standardInput.value = new LispStream(System.in, null)
      standardError.value = new LispStream(null, System.err)
    }
    catch {
      case e: UnsupportedEncodingException => ;
    }
    intern("Class").value = new JavaObject(classOf[Class[_]])
    intern("cons").value = new LispSubr("cons", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        cons(o(0), o(1))
      }
    }
    intern("car").value = new LispSubr("car", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        car(o(0).asInstanceOf[Cons])
      }
    }
    intern("cdr").value = new LispSubr("cdr", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        cdr(o(0).asInstanceOf[Cons])
      }
    }
    intern("rplaca").value = new LispSubr("rplaca", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[Cons]).setCar(o(1))
        o(0)
      }
    }
    intern("rplacd").value = new LispSubr("rplacd", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[Cons]).setCdr(o(1).asInstanceOf[Cons])
        o(0)
      }
    }
    intern("prin1").value = new LispSubr("prin1", 1, 2) {
      def apply(o: Array[LispObject]): LispObject = {
        prin1(o(0), if ((o.length > 1)) o(1).asInstanceOf[LispStream] else null)
      }
    }
    intern("eq?").value = new LispSubr("eq?", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        if (o(0) == o(1)) t else null // TODO: correct?
      }
    }
    intern("atom?").value = new LispSubr("atom?", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        atom(o(0))
      }
    }
    intern("set").value = new LispSubr("set", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[Symbol]).value = o(1)
        o(1)
      }
    }
    intern("eval").value = new LispSubr("eval", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        eval(o(0))
      }
    }
    intern("symbols").value = new LispSubr("symbols") {
      def apply(o: Array[LispObject]): LispObject = {
        symbols
      }
    }
    intern("symbol-value").value = new LispSubr("symbol-value", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        if ((o(0) == null)) null else symbolValue(o(0).asInstanceOf[Symbol])
      }
    }
    intern("gensym").value = new LispSubr("gensym") {
      def apply(o: Array[LispObject]): LispObject = {
        gensym
      }
    }
    intern("intern").value = new LispSubr("intern", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        if (o(0).isInstanceOf[LispString]) return intern((o(0).asInstanceOf[LispString]).toJavaString)
        if (o(0).isInstanceOf[Symbol]) return (o(0).asInstanceOf[Symbol]).intern
        throw new LispException(internalError, "Bad argument")
      }
    }
    intern("+").value = new LispSubr("+", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispNumber]).add(o(1).asInstanceOf[LispNumber])
      }
    }
    intern("-").value = new LispSubr("-", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispNumber]).sub(o(1).asInstanceOf[LispNumber])
      }
    }
    intern("*").value = new LispSubr("*", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispNumber]).mul(o(1).asInstanceOf[LispNumber])
      }
    }
    intern("/").value = new LispSubr("/", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispNumber]).div(o(1).asInstanceOf[LispNumber])
      }
    }
    intern("mod").value = new LispSubr("mod", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispInteger]).mod(o(1).asInstanceOf[LispInteger])
      }
    }
    intern("ash").value = new LispSubr("ash", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispInteger]).ash(o(1).asInstanceOf[LispInteger])
      }
    }
    intern("neg?").value = new LispSubr("neg?", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        if ((o(0).asInstanceOf[LispNumber]).negP) t else null
      }
    }
    intern("eql?").value = new LispSubr("eql?", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        eql(o(0), o(1))
      }
    }
    intern("=").value = new LispSubr("=", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        if (((o(0).asInstanceOf[LispNumber]) == o(1).asInstanceOf[LispNumber])) t else null
      }
    }
    intern("char=").value = new LispSubr("char=", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        if (((o(0).asInstanceOf[LispChar]).ch == (o(1).asInstanceOf[LispChar]).ch)) t else null
      }
    }
    intern("aref").value = new LispSubr("aref", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispArray]).aref((o(1).asInstanceOf[LispInteger]).toJavaInt)
      }
    }
    intern("aset").value = new LispSubr("aset", 3) {
      def apply(o: Array[LispObject]): LispObject = {
        (o(0).asInstanceOf[LispArray]).aset((o(1).asInstanceOf[LispInteger]).toJavaInt, o(2))
      }
    }
    intern("exit").value = new LispSubr("exit", 0, 1) {
      def apply(o: Array[LispObject]): LispObject = {
        System.exit(if ((o.length < 1)) 0 else (o(0).asInstanceOf[LispNumber]).toJavaInt)
        null
      }
    }
    intern("get-time").value = new LispSubr("get-time") {
      def apply(o: Array[LispObject]): LispObject = {
        new LispFixnum(System.currentTimeMillis)
      }
    }
    intern("read-char").value = new LispSubr("read-char", 0, 1) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          readChar(if ((o.length > 0)) o(0).asInstanceOf[LispStream] else null)
        }
        catch {
          case e: IOException => {
            throw new LispException(internalError, "An IOException just occured to me, " + this.toString)
          }
        }
      }
    }
    intern("write-char").value = new LispSubr("write-char", 1, 2) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          writeChar(o(0).asInstanceOf[LispChar], (if ((o.length > 1)) o(1).asInstanceOf[LispStream] else null))
        }
        catch {
          case e: IOException => {
            throw new LispException(internalError, "An IOException just occured to me, " + this.toString)
          }
        }
      }
    }
    intern("read").value = new LispSubr("read", 0, 1) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          read(if ((o.length > 0)) o(0).asInstanceOf[LispStream] else null)
        }
        catch {
          case e: IOException => {
            throw new LispException(internalError, "An IOException just ocurred to me, " + this.toString)
          }
        }
      }
    }
    intern("open").value = new LispSubr("open", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          if (o(1) eq in) return new LispStream(new FileReader((o(0).asInstanceOf[LispString]).toJavaString), null)
          if (o(1) eq out) return new LispStream(null, new PrintWriter(new FileWriter((o(0).asInstanceOf[LispString]).toJavaString)))
          throw new LispException(internalError, "You confused me, you want a stream out, or in?")
        }
        catch {
          case e: IOException => {
            throw new LispException(internalError, e)
          }
        }
      }
    }
    intern("close").value = new LispSubr("close", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          if ((o(0).asInstanceOf[LispStream]).close) t else null
        }
        catch {
          case e: IOException => {
            throw new LispException(internalError, "An IOException just ocurred to me, " + this.toString)
          }
        }
      }
    }
    intern("eof?").value = new LispSubr("eof?", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        if ((o(0).asInstanceOf[LispStream]).eof) t else null
      }
    }
    intern("make-runnable").value = new LispSubr("make-runnable", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new JavaObject(new Runnable {
          def run {
            eval(cons(o(0), null))
          }
        })
      }
    }
    intern("make-string-input-stream").value = new LispSubr("make-string-input-stream", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispStream(new StringReader((o(0).asInstanceOf[LispString]).toJavaString), null)
      }
    }
    intern("make-string-output-stream").value = new LispSubr("make-string-output-stream") {
      def apply(o: Array[LispObject]): LispObject = {
        new StringOutputStream
      }
    }
    intern("get-output-stream-string").value = new LispSubr("get-output-stream-string", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispString((o(0).asInstanceOf[StringOutputStream]).getOutputStreamString)
      }
    }
    intern("%try").value = new LispSubr("%try", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        try {
          eval(cons(o(0), null))
        }
        catch {
          case e: Exception => {
            eval(cons(o(1), cons(new JavaObject(e), null)))
          }
        }
      }
    }
    intern("throw").value = new LispSubr("throw", 1, 2) {
      def apply(o: Array[LispObject]): LispObject = {
        if (o.length == 2) {
          if (o(1).isInstanceOf[LispString]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[LispString]).toJavaString)
          else if (o(1).isInstanceOf[JavaObject]) throw new LispException(o(0).asInstanceOf[Symbol], (o(1).asInstanceOf[JavaObject]).getObj.asInstanceOf[Throwable])
          else throw new LispException(internalError, "Throw threw a throw.")
        }
        if (o(0).isInstanceOf[JavaObject] && (o(0).asInstanceOf[JavaObject]).getObj.isInstanceOf[LispException]) throw (o(0).asInstanceOf[JavaObject]).getObj.asInstanceOf[LispException]
        throw new LispException(o(0).asInstanceOf[Symbol])
      }
    }
    intern("make-array").value = new LispSubr("make-array", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        if (o(0).isInstanceOf[Cons]) return new LispArray(o(0).asInstanceOf[Cons])
        else if (o(0).isInstanceOf[LispInteger]) return new LispArray((o(0).asInstanceOf[LispInteger]).toJavaInt)
        else throw new LispException(internalError, "make-array wants an integer or a list")
      }
    }
    intern("make-string").value = new LispSubr("make-string", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispString((o(0).asInstanceOf[LispInteger]).toJavaInt, o(1).asInstanceOf[LispChar])
      }
    }
    intern("length").value = new LispSubr("length", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispFixnum(if ((o(0) == null)) 0 else if ((o(0).isInstanceOf[Cons])) (o(0).asInstanceOf[Cons]).length else (o(0).asInstanceOf[LispArray]).length)
      }
    }
    intern("equal?").value = new LispSubr("equal?", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        if ((if ((o(0) == null)) o(1) == null else (o(0) == o(1)))) t else null
      }
    }
    intern("sxhash").value = new LispSubr("sxhash", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispFixnum(if ((o(0) == null)) 0 else o(0).hashCode)
      }
    }
    intern("running-compiled?").value = new LispSubr("running-compiled?") {
      def apply(o: Array[LispObject]): LispObject = {
        null
      }
    }
    intern("char->integer").value = new LispSubr("char->integer", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispFixnum((o(0).asInstanceOf[LispChar]).ch.asInstanceOf[Int])
      }
    }
    intern("integer->char").value = new LispSubr("integer->char", 1) {
      def apply(o: Array[LispObject]): LispObject = {
        new LispChar((o(0).asInstanceOf[LispInteger]).toJavaInt.asInstanceOf[Char])
      }
    }
    intern("type?").value = new LispSubr("type?", 2) {
      def apply(o: Array[LispObject]): LispObject = {
        val knownType =
          if (o(0) eq number) o(1).isInstanceOf[LispNumber]
          else if (o(0) eq integer) o(1).isInstanceOf[LispInteger]
          else if (o(0) eq fixnum) o(1).isInstanceOf[LispFixnum]
          else if (o(0) eq bignum) o(1).isInstanceOf[LispBignum]
          else if (o(0) eq flonum) o(1).isInstanceOf[LispFlonum]
          else if (o(0) eq symbol) o(1).isInstanceOf[Symbol]
          else if (o(0) eq cons) o(1).isInstanceOf[Cons]
          else if (o(0) eq list) (o(1) == null || o(1).isInstanceOf[Cons])
          else if (o(0) eq procedure) o(1).isInstanceOf[Procedure]
          else if (o(0) eq subr) o(1).isInstanceOf[LispSubr]
          else if (o(0) eq array) o(1).isInstanceOf[LispArray]
          else if (o(0) eq string) o(1).isInstanceOf[LispString]
          else if (o(0) eq javaObject) o(1).isInstanceOf[JavaObject]
          else if (o(0) eq javaMethod) o(1).isInstanceOf[JavaMethod]
          else if (o(0) eq charmander) o(1).isInstanceOf[LispChar]
          else if (o(0) eq stream) o(1).isInstanceOf[LispStream]
          else false
        if (knownType) t else null
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
  }
}