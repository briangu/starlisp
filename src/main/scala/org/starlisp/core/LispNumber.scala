package org.starlisp.core

import java.math.BigInteger
import java.util.regex.Pattern

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