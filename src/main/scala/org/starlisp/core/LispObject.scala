package org.starlisp.core

import collection.mutable
import java.util
import collection.mutable.ListBuffer

object LispObject {
  def toStringOrNil(obj: LispObject): String = Option(obj).getOrElse("nil").toString
}

class LispObject {
  def as[T] = this.asInstanceOf[T]
}

abstract class Procedure(val name : String, val minArgs: Int = 0, val maxArgs: Int = Integer.MAX_VALUE) extends LispObject {
  def this(name: String, numArgs: Int) = this(name, numArgs, numArgs)

  protected def error(msg: String): LispObject = throw new LispException(Symbol.internalError, msg)

  def apply(env: Environment, list: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject

  override def toString: String = "#<subr %s >".format(name)
}

abstract class LispFn(name : String, minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)
  extends Procedure(name, minArgs, maxArgs)
{
  protected def evlisArray(list: Cell, env: Environment, length: Int, eval: ((LispObject, Environment) => LispObject)): (Array[LispObject],Int) = {
    if ((list eq null) || (length == 0)) {
      (Array(), 0)
    } else {
      val res = new ListBuffer[LispObject]
      var c = list
      while (c != null) {
        res.append(eval(c.car, env))
        c = c.rest
      }
      (res.toArray, res.size)
    }
  }

  def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
    val list = head.rest
    val (args, foundCount) = Option(list) match {
      case Some(argsList) => evlisArray(list, env, maxArgs, eval)
      case None => (Array[LispObject](), 0)
    }
    if (foundCount < minArgs) error("Too few args when calling procedure: " + toString)
    apply(env, args)
  }

  def apply(objects: Array[LispObject]): LispObject
  def apply(env: Environment, objects: Array[LispObject]): LispObject = apply(objects)
}

abstract class LispFn0[A <: LispObject](name : String) extends Procedure(name, 1, 1) {
  def apply(env: Environment, list: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
    apply()
  }

  def apply(): LispObject
}

abstract class LispFn1[A <: LispObject](name : String) extends Procedure(name, 1, 1) {
  def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
    val list = head.rest
    if (list eq null) error("Too few args when calling procedure: " + toString)
    apply(eval(list.car, env).asInstanceOf[A])
  }

  def apply(a: A): LispObject
}

abstract class LispFn2[A <: LispObject,B <: LispObject](name: String) extends Procedure(name, 2, 2) {
  def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
    val list = head.rest
    if (list eq null) error("Too few args when calling procedure: " + toString)
    val a = list.car
    if (list.rest eq null) error("Too few args when calling procedure: " + toString)
    val b = list.rest.first
    apply(eval(a, env).asInstanceOf[A], eval(b, env).asInstanceOf[B])
  }

  def apply(a: A, b: B): LispObject
}

// LispFnPair - same as LispFn2, but using the same types for A and B
abstract class LispFnP[A <: LispObject](name : String = "") extends LispFn2[A,A](name)

abstract class LispFn3[A <: LispObject,B <: LispObject,C <: LispObject](name : String = "") extends Procedure(name, 3, 3) {
  def apply(env: Environment, head: Cell, eval: ((LispObject, Environment) => LispObject)): LispObject = {
    val list = head.rest
    if (list eq null) error("Too few args when calling procedure: " + toString)
    val a = list.car
    if (list.rest eq null) error("Too few args when calling procedure: " + toString)
    val b = list.rest.first
    if (list.rest.rest eq null) error("Too few args when calling procedure: " + toString)
    val c = list.rest.rest.car
    apply(eval(a, env).asInstanceOf[A], eval(b, env).asInstanceOf[B], eval(c, env).asInstanceOf[C])
  }

  def apply(a: A, b: B, c: C): LispObject
}

class Cell(var car: LispObject = null, var cdr: LispObject = null) extends LispObject {

  def Car[T <: LispObject](): T = this.car.asInstanceOf[T]
  def Car(car: LispObject) : LispObject = { this.car = car; car }

  def first = Car[LispObject]
  def rest = Cdr[Cell]
  def cadr = rest.first
  def cddr = rest.rest
  def caddr = rest.rest.first
  def cdddr = rest.rest.rest

  def Cdr[T <: LispObject](): T = this.cdr.asInstanceOf[T]
  def Cdr(cdr: LispObject) : LispObject = { this.cdr = cdr; cdr }

  private final def hashCode(obj: LispObject): Int = {
    if ((obj == null)) 261835505 else if ((obj.isInstanceOf[Cell])) 1 + obj.hashCode else obj.hashCode
  }

  override def hashCode: Int = hashCode(car) + 31 * hashCode(cdr)
  private final def equals(a: LispObject, b: LispObject) = if (a == null) b == null else (a == b)
  override def equals(obj: Any): Boolean = {
    if (obj == null) {
      (car == null && cdr == null)
    } else {
      if (obj.isInstanceOf[Cell]) equals(obj.asInstanceOf[Cell].car, car) && equals(obj.asInstanceOf[Cell].cdr, cdr) else false
    }
  }

  def length: Int = {
    var i = 0
    var c = this
    while (c != null) {
      i += 1
      c = c.cdr.asInstanceOf[Cell] // TODO: what if not cons?
    }
    i
  }

  def toArray: Array[LispObject] = {
    val arr = new Array[LispObject](length)
    var c = this
    (0 until arr.length).foreach { i =>
      arr(i) = c.car
      c = c.cdr.asInstanceOf[Cell]
    }
    arr
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb.append("(")
    var list: Cell = this
    var done = false
    while (!done) {
      if (list.cdr == null) {
        sb.append(LispObject.toStringOrNil(list.car))
        done = true
      } else if (!(list.cdr.isInstanceOf[Cell])) {
        sb.append(LispObject.toStringOrNil(list.car)).append(" . ").append(list.cdr.toString)
        done = true
      } else {
        sb.append(LispObject.toStringOrNil(list.car)).append(" ")
        list = list.cdr.asInstanceOf[Cell]
      }
    }
    sb.append(")")
    sb.toString
  }
}

class LispArray(protected val ar: Array[LispObject]) extends LispObject {

  def this(length: Int) = this(new Array[LispObject](length))
  def this(list: Cell) = this(list.toArray)

  def length: Int = ar.length

  override def hashCode: Int = util.Arrays.deepHashCode(ar.asInstanceOf[Array[Object]])
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[LispArray] && util.Arrays.deepEquals(ar.asInstanceOf[Array[Object]], (obj.asInstanceOf[LispArray]).ar.asInstanceOf[Array[Object]])
  }

  def aref(idx: Int): LispObject = ar(idx)

  def aset(idx: Int, obj: LispObject): LispObject = {
    val res = ar(idx)
    ar(idx) = obj
    res
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("#(")
    for (o <- ar) {
      sb.append(LispObject.toStringOrNil(o))
      sb.append(' ')
    }
    sb.setLength(sb.length - 1)
    sb.append(')')
    sb.toString
  }
}

object LispChar {
  val lowCache = {
    val lc = new Array[LispChar](256)
    (0 until 256).foreach{idx => lc(idx) = new LispChar(idx.asInstanceOf[Char])}
    lc
  }

  val cache = new mutable.HashMap[Char, LispChar]()
  def create(ch: Char) = {
    if (ch < 256) {
      lowCache(ch)
    } else {
      cache.getOrElseUpdate(ch, new LispChar(ch))
    }
  }
}

class LispChar(val ch: Char = 0) extends LispObject {
  override def hashCode = ch.hashCode
  override def equals(obj: Any) = obj.isInstanceOf[LispChar] && (obj.asInstanceOf[LispChar]).ch == ch
  override def toString = "#\\%c".format(ch)
}

class LispString(length: Int) extends LispArray(length) {

  def this(arr: String) = {
    this(arr.length)
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr.charAt(idx)))
  }

  def this(arr: Array[Char], length: Int) = {
    this(length)
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr(idx)))
  }

  def this(arr: Array[Char]) = {
    this(arr, arr.length)
  }

  def this(length: Int, ch: LispChar) = {
    this(length)
    (0 until length).foreach(ar(_) = ch)
  }

  override def aset(idx: Int, obj: LispObject): LispObject = {
    if (!(obj.isInstanceOf[LispChar])) throw new LispException(Symbol.internalError, "Only Char may be in a string.")
    super.aset(idx, obj)
  }

  def toJavaString: String = {
    val sb: StringBuffer = new StringBuffer
    for (o <- ar) sb.append((o.asInstanceOf[LispChar]).ch)
    sb.toString
  }

  override def toString: String = "\"%s\"".format(toJavaString)
}

