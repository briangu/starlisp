package org.starlisp.core

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