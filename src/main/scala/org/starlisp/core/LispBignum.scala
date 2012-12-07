package org.starlisp.core

import java.math.BigInteger

object LispBignum {
  def parse(str: String): LispBignum = new LispBignum(new BigInteger(str))
}

class LispBignum(val n: BigInteger) extends LispInteger {
  def this(nbr: Long) {
    this(BigInteger.valueOf(nbr))
  }

  def this(nbr: Int) {
    this(BigInteger.valueOf(nbr.asInstanceOf[Long]))
  }

  override def hashCode: Int = n.hashCode
  override def equals(obj: Any): Boolean = {
    if ((obj.isInstanceOf[LispBignum]))
      (n == (obj.asInstanceOf[LispBignum]).n)
    else if ((obj.isInstanceOf[LispFixnum]))
      (this == coerceFixnumToBignum(obj.asInstanceOf[LispFixnum]))
    else if ((obj.isInstanceOf[LispFlonum]))
      (obj == this)
    else
      false
  }

  def add(nbr: LispBignum): LispBignum = new LispBignum(n.add(nbr.n))
  def sub(nbr: LispBignum): LispBignum = new LispBignum(n.subtract(nbr.n))
  def mul(nbr: LispBignum): LispBignum = new LispBignum(n.multiply(nbr.n))
  def div(nbr: LispBignum): LispNumber = new LispBignum(n.divide(nbr.n))
  def mod(nbr: LispBignum): LispBignum = new LispBignum(n.remainder(nbr.n))
  def ash(nbr: LispInteger): LispInteger = new LispBignum(n.shiftLeft(nbr.toJavaInt))
  def negP: Boolean = n.signum == -1

  def add(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.doubleValue)).add(nbr.asInstanceOf[LispFlonum]) else if ((nbr.isInstanceOf[LispFixnum])) add(coerceFixnumToBignum(nbr)) else add(nbr.asInstanceOf[LispBignum])
  }

  def sub(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.doubleValue)).sub(nbr.asInstanceOf[LispFlonum]) else if ((nbr.isInstanceOf[LispFixnum])) sub(coerceFixnumToBignum(nbr)) else sub(nbr.asInstanceOf[LispBignum])
  }

  def mul(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.doubleValue)).mul(nbr.asInstanceOf[LispFlonum]) else if ((nbr.isInstanceOf[LispFixnum])) mul(coerceFixnumToBignum(nbr)) else mul(nbr.asInstanceOf[LispBignum])
  }

  def div(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.doubleValue)).div(nbr.asInstanceOf[LispFlonum]) else if ((nbr.isInstanceOf[LispFixnum])) div(coerceFixnumToBignum(nbr)) else div(nbr.asInstanceOf[LispBignum])
  }

  def mod(nbr: LispInteger): LispInteger = {
    if ((nbr.isInstanceOf[LispFixnum])) mod(coerceFixnumToBignum(nbr)) else mod(nbr.asInstanceOf[LispBignum])
  }

  override def toString: String = n.toString

  def toJavaInt: Int = n.intValue
  def toJavaLong: Long = n.longValue
  def toJavaFloat: Float = n.floatValue
  def toJavaDouble: Double = n.doubleValue
  def toJavaBigInteger: BigInteger = n
}