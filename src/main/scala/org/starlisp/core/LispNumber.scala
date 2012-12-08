package org.starlisp.core

import java.util.regex.Pattern
import java.math.BigInteger

object LispNumber {

  final val REGEX: Pattern = Pattern.compile("^[+-]?\\d*\\.?(?:\\d+e)?\\d+$")

  final def isNumber(str: String): Boolean = REGEX.matcher(str).matches

  def tryParse(str: String): LispNumber = {
    try {
      LispFixnum.parse(str)
    } catch {
      case e: NumberFormatException => {
        try {
          LispBignum.parse(str)
        }
        catch {
          case ee: NumberFormatException => {
            LispFlonum.parse(str)
          }
        }
      }
    }
  }
}

abstract class LispNumber extends LispObject {
  protected final def coerceFixnumToBignum(nbr: LispNumber): LispBignum = {
    new LispBignum(nbr.toJavaLong)
  }

  protected final def coerceIntegerToFlonum(nbr: LispNumber): LispFlonum = {
    new LispFlonum(nbr.toJavaDouble)
  }

  def add(n: LispNumber): LispNumber

  def sub(n: LispNumber): LispNumber

  def mul(n: LispNumber): LispNumber

  def div(n: LispNumber): LispNumber

  def negP: Boolean

  def toJavaInt: Int

  def toJavaLong: Long

  def toJavaFloat: Float

  def toJavaDouble: Double

  def toJavaBigInteger: BigInteger
}

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

abstract class LispInteger extends LispNumber {
  def mod(n: LispInteger): LispInteger
  def ash(n: LispInteger): LispInteger
}

object LispFixnum {
  def parse(str: String): LispFixnum = new LispFixnum(str.toLong)

  def create(n: Long): LispFixnum = {
    if ((n < 65536)) cache(n.asInstanceOf[Int]) else new LispFixnum(n)
  }

  private[core] var cache: Array[LispFixnum] = new Array[LispFixnum](65536)

  (0 until cache.length).foreach { idx => cache(idx) = new LispFixnum(idx)}
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

  def div(nbr: LispFixnum): LispNumber = LispFixnum.create(n / nbr.n)

  def mod(nbr: LispFixnum): LispInteger = LispFixnum.create(n % nbr.n)

  def ash(nbr: LispInteger): LispInteger = ash(nbr.asInstanceOf[LispFixnum])

  def negP: Boolean = n < 0

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

  override def toString: String = String.valueOf(n)

  def toJavaInt: Int = n.asInstanceOf[Int]

  def toJavaLong: Long = n

  def toJavaFloat: Float = n.asInstanceOf[Float]

  def toJavaDouble: Double = n.asInstanceOf[Double]

  def toJavaBigInteger: BigInteger = BigInteger.valueOf(n)
}

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