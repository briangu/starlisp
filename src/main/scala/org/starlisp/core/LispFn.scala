package org.starlisp.core

import scala.Array

abstract class LispFn(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)
  extends Procedure(name, minArgs, maxArgs) {}

object LispFn {
  def apply(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)(t:(Array[LispObject]) => LispObject) : LispObject = {
    def f(arr: Array[LispObject]): LispObject = t(arr)
    new LispSubrImplicit(name, minArgs, maxArgs)(f)
  }
}

class LispSubrImplicit(name : String, minArgs: Int, maxArgs: Int)(t:(Array[LispObject]) => LispObject)
  extends Procedure(name, minArgs, maxArgs) {
  def apply(objects: Array[LispObject]) : LispObject = t(objects)
}

object LispFn0 {
  def apply(name : String = "")(t:() => LispObject) : LispObject = {
    def f(): LispObject = t()
    new LispFn0Implicit(name, 0, 0)(f)
  }
}

class LispFn0Implicit[A](name : String, minArgs: Int, maxArgs: Int)(t:() => LispObject)
  extends Procedure(name, minArgs, maxArgs) {
  def apply(objects: Array[LispObject]) : LispObject = t()
}

object LispFn1 {
  def apply[A](name : String = "")(t:(A) => LispObject) : LispObject = {
    def f(a: A): LispObject = t(a)
    new LispFn1Implicit[A](name, 1, 1)(f)
  }
}

class LispFn1Implicit[A](name : String, minArgs: Int, maxArgs: Int)(t:(A) => LispObject)
  extends Procedure(name, minArgs, maxArgs) {
  def apply(objects: Array[LispObject]) : LispObject = t(objects(0).asInstanceOf[A])
}

object LispFn2 {
  def apply[A](name : String = "")(t:(A,A) => LispObject) : LispObject = {
    def f(a: A, b: A): LispObject = t(a, b)
    new LispFn2Implicit[A,A](name, 2, 2)(f)
  }
}

// mixed types
object LispFn2M {
  def apply[A,B](name : String = "")(t:(A,B) => LispObject) : LispObject = {
    def f(a: A, b: B): LispObject = t(a, b)
    new LispFn2Implicit[A,B](name, 2, 2)(f)
  }
}

class LispFn2Implicit[A,B](name : String, minArgs: Int, maxArgs: Int)(t:(A,B) => LispObject)
  extends Procedure(name, minArgs, maxArgs) {
  def apply(objects: Array[LispObject]) : LispObject = t(objects(0).asInstanceOf[A], objects(1).asInstanceOf[B])
}
