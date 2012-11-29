package org.starlisp.core

class Cons(var car: LispObject = null, var cdr: LispObject = null) extends LispObject {

  def setCar(car: LispObject) : LispObject = {
    this.car = car
    car
  }

  def setCdr(cdr: LispObject) : LispObject = {
    this.cdr = cdr
    cdr
  }

  private final def hashCode(obj: LispObject): Int = {
    if ((obj == null)) 261835505 else if ((obj.isInstanceOf[Cons])) 1 + obj.hashCode else obj.hashCode
  }

  override def hashCode: Int = {
    hashCode(car) + 31 * hashCode(cdr)
  }

  private final def equals(a: LispObject, b: LispObject): Boolean = {
    if ((a == null)) b == null else (a == b)
  }

  override def equals(obj: Any): Boolean = {
    if ((obj.isInstanceOf[Cons])) equals((obj.asInstanceOf[Cons]).car, car) && equals((obj.asInstanceOf[Cons]).cdr, cdr) else false
  }

  def length: Int = {
    var i = 0
    var c = this
    while (c != null) {
      i += 1
      c = c.cdr.asInstanceOf[Cons] // TODO: what if not cons?
    }
    i
  }

  def toArray: Array[LispObject] = {
    val arr = new Array[LispObject](length)
    var c = this
    (0 until arr.length).foreach { i =>
      arr(i) = c
      c = c.cdr.asInstanceOf[Cons]
    }
    arr
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb.append("(")
    var list: Cons = this
    var done = false
    while (!done) {
      if (list.cdr == null) {
        sb.append(Starlisp.toStringOrNull(list.car))
        done = true
      } else if (!(list.cdr.isInstanceOf[Cons])) {
        sb.append(Starlisp.toStringOrNull(list.car)).append(" . ").append(list.cdr.toString)
        done = true
      } else {
        sb.append(Starlisp.toStringOrNull(list.car)).append(" ")
        list = list.cdr.asInstanceOf[Cons]
      }
    }
    sb.append(")")
    sb.toString
  }
}