package org.starlisp.core

import java.util.Arrays
import collection.mutable

object LispObject {
  def toStringOrNil(obj: LispObject): String = Option(obj).getOrElse("nil").toString
}

class LispObject {
  def as[T] = this.asInstanceOf[T]
}

abstract class Procedure(val name : String = "", val minArgs: Int = 0, val maxArgs: Int = Integer.MAX_VALUE) extends LispObject {
  def this(name: String, numArgs: Int) = this(name, numArgs, numArgs)

  def apply(env: Environment, objects: Array[LispObject]): LispObject

  override def toString: String = "#<subr %s >".format(name)
}

abstract class LispFn(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)
  extends Procedure(name, minArgs, maxArgs)
{
  def apply(objects: Array[LispObject]): LispObject
  def apply(env: Environment, objects: Array[LispObject]): LispObject = apply(objects)
}

abstract class LispFn1[A <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
}

abstract class LispFn2[A <: LispObject,B <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
  def b(o: Array[LispObject]): B = o(1).asInstanceOf[B]
}

// LispFnPair - same as LispFn2, but using the same types for A and B
abstract class LispFnP[A <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn2[A,A](name, minArgs, maxArgs)

abstract class LispFn3[A <: LispObject,B <: LispObject,C <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
  def b(o: Array[LispObject]): B = o(1).asInstanceOf[B]
  def c(o: Array[LispObject]): C = o(2).asInstanceOf[C]
}

class Cell(var car: LispObject = null, var cdr: LispObject = null) extends LispObject {

  def Car[T <: LispObject](): T = this.car.asInstanceOf[T]
  def Car(car: LispObject) : LispObject = { this.car = car; car }

  def first = Car[LispObject]
  def rest = Cdr[Cell]
  def cadr = rest.first
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

  override def hashCode: Int = Arrays.deepHashCode(ar.asInstanceOf[Array[Object]])
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[LispArray] && Arrays.deepEquals(ar.asInstanceOf[Array[Object]], (obj.asInstanceOf[LispArray]).ar.asInstanceOf[Array[Object]])
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

