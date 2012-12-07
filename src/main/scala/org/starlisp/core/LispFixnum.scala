package org.starlisp.core

import java.math.BigInteger

object LispFixnum {
  def parse(str: String): LispFixnum = new LispFixnum(str.toLong)

  def create(n: Long): LispFixnum = {
    if ((n < 65536)) cache(n.asInstanceOf[Int]) else new LispFixnum(n)
  }

  private[core] var cache: Array[LispFixnum] = new Array[LispFixnum](65536)

  (0 until cache.length).foreach{idx => cache(idx) = new LispFixnum(idx)}
}

final class LispFixnum(val n: Long) extends LispInteger {

  override def hashCode: Int = n.hashCode()
  override def equals(obj: Any): Boolean = {
    if ((obj.isInstanceOf[LispFixnum]))
      n == (obj.asInstanceOf[LispFixnum]).n
    else if ((obj.isInstanceOf[LispNumber]))
      (obj == this)
    else
      false
  }

  def add(nbr: LispFixnum): LispInteger = {
    val res = LispFixnum.create(n + nbr.n)
    if (((this.n ^ res.n) & (nbr.n ^ res.n)) < 0) {
      return (new LispBignum(n)).add(new LispBignum(nbr.n))
    }
    res
  }

  def sub(nbr: LispFixnum): LispInteger = {
    val res: LispFixnum = LispFixnum.create(n - nbr.n)
    if (((this.n ^ res.n) & (-nbr.n ^ res.n)) < 0) {
      return (new LispBignum(n)).sub(new LispBignum(nbr.n))
    }
    res
  }

  def mul(nbr: LispFixnum): LispInteger = {
    if (java.lang.Long.numberOfLeadingZeros(Math.abs(n)) + java.lang.Long.numberOfLeadingZeros(Math.abs(nbr.n)) < 65) {
      return (new LispBignum(n)).mul(new LispBignum(nbr.n))
    }
    LispFixnum.create(n * nbr.n)
  }

  def div(nbr: LispFixnum): LispNumber = {
    LispFixnum.create(n / nbr.n)
  }

  def mod(nbr: LispFixnum): LispInteger = {
    LispFixnum.create(n % nbr.n)
  }

  def ash(nbr: LispFixnum): LispInteger = {
    LispFixnum.create(if ((nbr.n > 0)) n << nbr.n else n >> -nbr.n)
  }

  def add(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispBignum])) (new LispBignum(n)).add(nbr.asInstanceOf[LispBignum]) else if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.asInstanceOf[Double])).add(nbr.asInstanceOf[LispFlonum]) else add(nbr.asInstanceOf[LispFixnum])
  }

  def sub(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispBignum])) (new LispBignum(n)).sub(nbr.asInstanceOf[LispBignum]) else if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.asInstanceOf[Double])).sub(nbr.asInstanceOf[LispFlonum]) else sub(nbr.asInstanceOf[LispFixnum])
  }

  def mul(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispBignum])) (new LispBignum(n)).mul(nbr.asInstanceOf[LispBignum]) else if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.asInstanceOf[Double]).mul(nbr.asInstanceOf[LispFlonum])) else mul(nbr.asInstanceOf[LispFixnum])
  }

  def div(nbr: LispNumber): LispNumber = {
    if ((nbr.isInstanceOf[LispBignum])) (new LispBignum(n)).div(nbr.asInstanceOf[LispBignum]) else if ((nbr.isInstanceOf[LispFlonum])) (new LispFlonum(n.asInstanceOf[Double])).div(nbr.asInstanceOf[LispFlonum]) else div(nbr.asInstanceOf[LispFixnum])
  }

  def mod(nbr: LispInteger): LispInteger = {
    if ((nbr.isInstanceOf[LispBignum])) (new LispBignum(n)).mod(nbr.asInstanceOf[LispBignum]) else mod(nbr.asInstanceOf[LispFixnum])
  }

  def ash(nbr: LispInteger): LispInteger = {
    ash(nbr.asInstanceOf[LispFixnum])
  }

  def negP: Boolean = n < 0

  override def toString: String = String.valueOf(n)

  def toJavaInt: Int = n.asInstanceOf[Int]
  def toJavaLong: Long = n
  def toJavaFloat: Float = n.asInstanceOf[Float]
  def toJavaDouble: Double = n.asInstanceOf[Double]
  def toJavaBigInteger: BigInteger = BigInteger.valueOf(n)
}