package org.starlisp.core

import java.math.BigInteger

object LispFlonum {
  def parse(str: String): LispNumber = new LispFlonum(str.toDouble)
}

final class LispFlonum(val n: Double) extends LispNumber {

  override def hashCode: Int = n.hashCode

  def add(nbr: LispFlonum): LispFlonum = new LispFlonum(n + nbr.n)
  def sub(nbr: LispFlonum): LispFlonum = new LispFlonum(n - nbr.n)
  def mul(nbr: LispFlonum): LispFlonum = new LispFlonum(n * nbr.n)
  def div(nbr: LispFlonum): LispFlonum = new LispFlonum(n / nbr.n)
  def negP: Boolean = n < 0

  override def equals(obj: Any): Boolean = {
    if ((obj.isInstanceOf[LispFlonum])) n == (obj.asInstanceOf[LispFlonum]).n else if ((obj.isInstanceOf[LispInteger])) n == (obj.asInstanceOf[LispInteger]).toJavaDouble else false
  }

  def add(nbr: LispNumber): LispFlonum = {
    if ((nbr.isInstanceOf[LispInteger])) add(coerceIntegerToFlonum(nbr)) else add(nbr.asInstanceOf[LispFlonum])
  }

  def sub(nbr: LispNumber): LispFlonum = {
    if ((nbr.isInstanceOf[LispInteger])) sub(coerceIntegerToFlonum(nbr)) else sub(nbr.asInstanceOf[LispFlonum])
  }

  def mul(nbr: LispNumber): LispFlonum = {
    if ((nbr.isInstanceOf[LispInteger])) mul(coerceIntegerToFlonum(nbr)) else mul(nbr.asInstanceOf[LispFlonum])
  }

  def div(nbr: LispNumber): LispFlonum = {
    if ((nbr.isInstanceOf[LispInteger])) div(coerceIntegerToFlonum(nbr)) else div(nbr.asInstanceOf[LispFlonum])
  }

  override def toString: String = String.valueOf(n)

  def toJavaInt: Int = n.asInstanceOf[Int]
  def toJavaLong: Long = n.asInstanceOf[Long]
  def toJavaFloat: Float = n.asInstanceOf[Float]
  def toJavaDouble: Double = n
  def toJavaBigInteger: BigInteger = BigInteger.valueOf(n.asInstanceOf[Long])
}