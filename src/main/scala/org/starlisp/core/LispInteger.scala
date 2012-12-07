package org.starlisp.core

abstract class LispInteger extends LispNumber {
  def mod(n: LispInteger): LispInteger
  def ash(n: LispInteger): LispInteger
}