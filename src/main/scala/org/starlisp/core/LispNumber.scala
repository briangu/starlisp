package org.starlisp.core

import java.util.regex.Pattern
import java.math.BigInteger
import annotation.switch

object LispNumber {

  val REGEX: Pattern = Pattern.compile("^[+-]?\\d*\\.?(?:\\d+e)?\\d+$")

  def isNumber(str: String): Boolean = REGEX.matcher(str).matches

  def tryParse(str: String): LispNumber = {
    try {
      new LispFixnum(str.toLong)
    } catch {
      case e: NumberFormatException => {
        try {
          new LispBigInt(BigInt(str))
        }
        catch {
          case ee: NumberFormatException => {
            new LispFlonum(str.toDouble)
          }
        }
      }
    }
  }
}

object Calculate {
  def add(a: LispNumber, b: LispNumber) : LispNumber = (b(a), a(b)) match {
    case (a: LispBigDecimal, b: LispBigDecimal) => new LispBigDecimal(a.toBigDecimal + b.toBigDecimal)
    case (a: LispBigInt, b: LispBigInt) => new LispBigInt(a.toBigInt + b.toBigInt)
    case (a: LispFlonum, b: LispFlonum) => new LispFlonum(a.toJavaDouble + b.toJavaDouble)
    case (a: LispFixnum, b: LispFixnum) => {
      val n = a.toJavaLong
      val nbr = b.toJavaLong
      val res = LispFixnum.create(n + nbr)
      if (((n ^ res.n) & (nbr ^ res.n)) < 0) new LispBigInt(a.toBigInt + b.toBigInt) else res
    }
    case _ => throw new UnsupportedOperationException("unsupported: %s + %s".format(a.getClass.getName, b.getClass.getName))
  }

  def sub(a: LispNumber, b: LispNumber) : LispNumber = (b(a), a(b)) match {
    case (a: LispBigDecimal, b: LispBigDecimal) => new LispBigDecimal(a.toBigDecimal - b.toBigDecimal)
    case (a: LispBigInt, b: LispBigInt) => new LispBigInt(a.toBigInt - b.toBigInt)
    case (a: LispFlonum, b: LispFlonum) => new LispFlonum(a.toJavaDouble - b.toJavaDouble)
    case (a: LispFixnum, b: LispFixnum) =>  {
      val n = a.toJavaLong
      val nbr = b.toJavaLong
      val res: LispFixnum = LispFixnum.create(n - nbr)
      if (((n ^ res.n) & (-nbr ^ res.n)) < 0) new LispBigInt(a.toBigInt - b.toBigInt) else res
    }
    case _ => throw new UnsupportedOperationException("unsupported: %s + %s".format(a.getClass.getName, b.getClass.getName))
  }

  def mul(a: LispNumber, b: LispNumber) : LispNumber = (b(a), a(b)) match {
    case (a: LispBigDecimal, b: LispBigDecimal) => new LispBigDecimal(a.toBigDecimal * b.toBigDecimal)
    case (a: LispBigInt, b: LispBigInt) => new LispBigInt(a.toBigInt * b.toBigInt)
    case (a: LispFlonum, b: LispFlonum) => new LispFlonum(a.toJavaDouble * b.toJavaDouble)
    case (a: LispFixnum, b: LispFixnum) => {
      val n = a.toJavaLong
      val nbr = b.toJavaLong
      if (java.lang.Long.numberOfLeadingZeros(Math.abs(n)) + java.lang.Long.numberOfLeadingZeros(Math.abs(nbr)) < 65)
        new LispBigInt(a.toBigInt * b.toBigInt)
      else
        LispFixnum.create(n * nbr)
    }
    case _ => throw new UnsupportedOperationException("unsupported: %s + %s".format(a.getClass.getName, b.getClass.getName))
  }

  def div(a: LispNumber, b: LispNumber) : LispNumber = (b(a), a(b)) match {
    case (a: LispBigDecimal, b: LispBigDecimal) => new LispBigDecimal(a.toBigDecimal / b.toBigDecimal)
    case (a: LispBigInt, b: LispBigInt) => new LispBigInt(a.toBigInt / b.toBigInt)
    case (a: LispFlonum, b: LispFlonum) => new LispFlonum(a.toJavaDouble / b.toJavaDouble)
    case (a: LispFixnum, b: LispFixnum) => new LispFixnum(a.toJavaLong / b.toJavaLong)
    case _ => throw new UnsupportedOperationException("unsupported: %s + %s".format(a.getClass.getName, b.getClass.getName))
  }
}

abstract class LispNumber extends LispObject {
  protected def coerceFixnumToBignum(nbr: LispNumber) = new LispBigInt(nbr.toJavaLong)

  def add(n: LispNumber) = Calculate.add(this, n)
  def sub(n: LispNumber) = Calculate.sub(this, n)
  def mul(n: LispNumber) = Calculate.mul(this, n)
  def div(n: LispNumber) = Calculate.div(this, n)

  def negP: Boolean

  def apply(n: LispNumber): LispNumber

  def toJavaInt: Int
  def toJavaLong: Long
  def toJavaFloat: Float
  def toJavaDouble: Double
  def toJavaBigInteger: BigInteger
  def toBigDecimal: BigDecimal
  def toBigInt: BigInt
}

abstract class LispInteger extends LispNumber {
  def mod(n: LispInteger): LispInteger
  def ash(n: LispInteger): LispInteger
}

class LispBigDecimal(val n: BigDecimal) extends LispNumber {
  def this(nbr: BigInt) = this(BigDecimal(nbr))

  def negP = n.signum == -1

  def apply(n: LispNumber) = n match {
    case o: LispBigDecimal => o
    case o: LispNumber => new LispBigDecimal(o.toBigDecimal)
  }

  def toJavaInt = n.intValue()
  def toJavaLong = n.longValue()
  def toJavaFloat = n.floatValue()
  def toJavaDouble = n.doubleValue()
  def toJavaBigInteger = n.bigDecimal.toBigInteger()
  def toBigDecimal = n
  def toBigInt = new BigInt(n.bigDecimal.toBigInteger)

  override def toString: String = n.toString
  override def hashCode: Int = n.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case o: LispBigDecimal => (n == o.toBigDecimal)
    case _ => false
  }
}

class LispBigInt(val n: BigInt) extends LispInteger {
  def this(obj: BigInteger) = this(new BigInt(obj))

  def negP: Boolean = n.signum == -1
  def ash(nbr: LispInteger) = new LispBigInt(n << nbr.toJavaInt)
  def mod(nbr: LispInteger): LispInteger = new LispBigInt(n % nbr.toBigInt)

  def apply(n: LispNumber) = n match {
    case o: LispBigDecimal => o
    case o: LispBigInt => o
    case o: LispNumber => new LispBigInt(o.toBigInt)
  }

  def toJavaInt: Int = n.intValue
  def toJavaLong: Long = n.longValue
  def toJavaFloat: Float = n.floatValue
  def toJavaDouble: Double = n.doubleValue
  def toJavaBigInteger: BigInteger = n.bigInteger
  def toBigDecimal: BigDecimal = BigDecimal(n)
  def toBigInt = n

  override def toString: String = n.toString
  override def hashCode: Int = n.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case o:LispBigInt => (n == o.toBigInt)
    case _ => false
  }
}

object LispFixnum {

  private val cache: Array[LispFixnum] = new Array[LispFixnum](65536)

  def create(n: Long): LispFixnum = {
    if (n >= -32768 && n < 32768) {
      if (cache(n.toInt) == null) {
        cache(n.toInt) = new LispFixnum(n)
      }
      cache(n.toInt)
    } else {
      new LispFixnum(n)
    }
  }
}

final class LispFixnum(val n: Long) extends LispInteger {

  def negP: Boolean = n < 0
  def mod(nbr: LispInteger): LispInteger = LispFixnum.create(n % nbr.toJavaLong)
  def ash(nbr: LispInteger): LispInteger = {
    val x = nbr.toJavaLong
    LispFixnum.create(if ((x > 0)) n << x else n >> -x)
  }

  def apply(n: LispNumber) = n

  def toJavaInt: Int = n.toInt
  def toJavaLong: Long = n
  def toJavaFloat: Float = n.toFloat
  def toJavaDouble: Double = n.toDouble
  def toJavaBigInteger: BigInteger = BigInteger.valueOf(n)
  def toBigDecimal = BigDecimal(n)
  def toBigInt = BigInt(n)

  override def toString: String = String.valueOf(n)
  override def hashCode: Int = n.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case o:LispNumber => n == o.toJavaDouble
    case _ => false
  }
}

final class LispFlonum(val n: Double) extends LispNumber {

  def negP: Boolean = n < 0

  def apply(n: LispNumber) = n match {
    case o: LispFixnum => new LispFlonum(o.toJavaDouble)
    case o: LispNumber => o
  }

  def toJavaInt: Int = n.toInt
  def toJavaLong: Long = n.toLong
  def toJavaFloat: Float = n.toFloat
  def toJavaDouble: Double = n
  def toJavaBigInteger: BigInteger = BigInteger.valueOf(n.toLong)
  def toBigDecimal = BigDecimal(n)
  def toBigInt = BigInt(n.toLong)

  override def toString: String = String.valueOf(n)
  override def hashCode: Int = n.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case o: LispNumber => n == o.toJavaDouble
    case _ => false
  }
}