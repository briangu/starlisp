package org.starlisp.core

import java.util.regex.Pattern
import java.math.BigInteger

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

abstract class LispNumber extends LispObject {
  def add(n: LispNumber) : LispNumber
  def sub(n: LispNumber) : LispNumber
  def mul(n: LispNumber) : LispNumber
  def div(n: LispNumber) : LispNumber
  def negP: Boolean

  def promote(n: LispNumber): LispNumber

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

  override def add(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigDecimal])
      new LispBigDecimal(n + nbr.asInstanceOf[LispBigDecimal].n )
    else
      nbr.promote(this).add(promote(nbr))
  }
  override def sub(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigDecimal])
      new LispBigDecimal(n - nbr.asInstanceOf[LispBigDecimal].n )
    else
      nbr.promote(this).sub(promote(nbr))
  }
  override def mul(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigDecimal])
      new LispBigDecimal(n * nbr.asInstanceOf[LispBigDecimal].n )
    else
      nbr.promote(this).mul(promote(nbr))
  }
  override def div(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigDecimal])
      new LispBigDecimal(n / nbr.asInstanceOf[LispBigDecimal].n )
    else
      nbr.promote(this).div(promote(nbr))
  }
  def negP = n.signum == -1

  def promote(n: LispNumber) = n match {
    case o: LispBigDecimal => o
    case o: LispNumber => new LispBigDecimal(o.toBigDecimal)
  }

  def toJavaInt = n.intValue()
  def toJavaLong = n.longValue()
  def toJavaFloat = n.floatValue()
  def toJavaDouble = n.doubleValue()
  def toJavaBigInteger = n.bigDecimal.toBigInteger
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

  override def add(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigInt])
      new LispBigInt(n + nbr.asInstanceOf[LispBigInt].n )
    else
      nbr.promote(this).add(promote(nbr))
  }
  override def sub(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigInt])
      new LispBigInt(n - nbr.asInstanceOf[LispBigInt].n )
    else
      nbr.promote(this).sub(promote(nbr))
  }
  override def mul(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigInt])
      new LispBigInt(n * nbr.asInstanceOf[LispBigInt].n )
    else
      nbr.promote(this).mul(promote(nbr))
  }
  override def div(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispBigInt])
      new LispBigInt(n / nbr.asInstanceOf[LispBigInt].n )
    else
      nbr.promote(this).div(promote(nbr))
  }
  def negP: Boolean = n.signum == -1
  def ash(nbr: LispInteger) = new LispBigInt(n << nbr.toJavaInt)
  def mod(nbr: LispInteger): LispInteger = new LispBigInt(n % nbr.toBigInt)

  def promote(n: LispNumber) = n match {
    case o: LispBigDecimal => o
    case o: LispBigInt => o
    case o: LispFlonum => new LispBigDecimal(o.toBigDecimal)
    case o: LispFixnum => new LispBigInt(o.toBigInt)
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

class LispFlonum(val n: Double) extends LispNumber {

  override def add(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFlonum])
      new LispFlonum(n + nbr.asInstanceOf[LispFlonum].n)
    else
      nbr.promote(this).add(promote(nbr))
  }
  override def sub(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFlonum])
      new LispFlonum(n - nbr.asInstanceOf[LispFlonum].n)
    else
      nbr.promote(this).sub(promote(nbr))
  }
  override def mul(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFlonum])
      new LispFlonum(n * nbr.asInstanceOf[LispFlonum].n)
    else
      nbr.promote(this).mul(promote(nbr))
  }
  override def div(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFlonum])
      new LispFlonum(n / nbr.asInstanceOf[LispFlonum].n)
    else
      nbr.promote(this).div(promote(nbr))
  }
  def negP: Boolean = n < 0

  def promote(n: LispNumber) = n match {
    case o: LispBigInt => new LispBigDecimal(o.toBigDecimal)
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

object LispFixnum {

  private val minCached = -32768
  private val maxCached = 32768
  private val cache: Array[LispFixnum] = new Array[LispFixnum](maxCached - minCached + 1)

  def create(i: Long): LispFixnum = {
    if (minCached <= i && i <= maxCached) {
      val offset = i.toInt - minCached
      var n = cache(offset)
      if (n eq null) { n = new LispFixnum(i); cache(offset) = n }
      n
    } else {
      new LispFixnum(i)
    }
  }
}

class LispFixnum(val n: Long) extends LispInteger {

  override def add(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFixnum]) {
      val b = nbr.asInstanceOf[LispFixnum]
      val res = LispFixnum.create(n + b.n)
      if (((n ^ res.n) & (b.n ^ res.n)) < 0) new LispBigInt(toBigInt + b.toBigInt) else res
    } else {
      nbr.promote(this).add(nbr) // skip promote(nbr) since fixnum is at the bottom
    }
  }
  override def sub(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFixnum]) {
      val b = nbr.asInstanceOf[LispFixnum]
      val res: LispFixnum = LispFixnum.create(n - b.n)
      if (((n ^ res.n) & (-b.n ^ res.n)) < 0) new LispBigInt(toBigInt - b.toBigInt) else res
    } else {
      nbr.promote(this).sub(nbr) // skip promote(nbr) since fixnum is at the bottom
    }
  }
  override def mul(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFixnum]) {
      import java.lang.Long
      val b = nbr.asInstanceOf[LispFixnum]
      if (Long.numberOfLeadingZeros(math.abs(n)) + Long.numberOfLeadingZeros(math.abs(b.n)) < 65)
        new LispBigInt(toBigInt * b.toBigInt)
      else
        LispFixnum.create(n * b.n)
    } else {
      nbr.promote(this).mul(nbr) // skip promote(nbr) since fixnum is at the bottom
    }
  }
  override def div(nbr: LispNumber) = {
    if (nbr.isInstanceOf[LispFixnum])
      LispFixnum.create(n / nbr.asInstanceOf[LispFixnum].n )
    else
      nbr.promote(this).div(nbr) // skip promote(nbr) since fixnum is at the bottom
  }
  def negP: Boolean = n < 0
  def mod(nbr: LispInteger): LispInteger = LispFixnum.create(n % nbr.toJavaLong)
  def ash(nbr: LispInteger): LispInteger = {
    val x = nbr.toJavaLong
    LispFixnum.create(if (x > 0) n << x else n >> -x)
  }

  def promote(n: LispNumber) = n

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

