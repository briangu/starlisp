package org.starlisp.core;

import java.math.BigInteger;

public final class LispFlonum extends LispNumber {
  private double n;

  public static LispNumber parse(String str) {
    return new LispFlonum(Double.parseDouble(str));
  }

  public LispFlonum(double nbr) {
    n = nbr;
  }

  public int hashCode() {
    return Double.valueOf(n).hashCode();
  }

  public LispFlonum add(LispFlonum nbr) {
    return new LispFlonum(n + nbr.n);
  }

  public LispFlonum sub(LispFlonum nbr) {
    return new LispFlonum(n - nbr.n);
  }

  public LispFlonum mul(LispFlonum nbr) {
    return new LispFlonum(n * nbr.n);
  }

  public LispFlonum div(LispFlonum nbr) {
    return new LispFlonum(n / nbr.n);
  }

  public boolean negP() {
    return n < 0;
  }

  public boolean equals(Object obj) {
    return (obj instanceof LispFlonum) ? n == ((LispFlonum) obj).n :
        (obj instanceof LispInteger) ? n == ((LispInteger) obj).toJavaDouble() :
            false;
  }

  public LispFlonum add(LispNumber nbr) {
    return (nbr instanceof LispInteger) ? add(coerceIntegerToFlonum(nbr)) : add((LispFlonum) nbr);
  }

  public LispFlonum sub(LispNumber nbr) {
    return (nbr instanceof LispInteger) ? sub(coerceIntegerToFlonum(nbr)) : sub((LispFlonum) nbr);
  }

  public LispFlonum mul(LispNumber nbr) {
    return (nbr instanceof LispInteger) ? mul(coerceIntegerToFlonum(nbr)) : mul((LispFlonum) nbr);
  }

  public LispFlonum div(LispNumber nbr) {
    return (nbr instanceof LispInteger) ? div(coerceIntegerToFlonum(nbr)) : div((LispFlonum) nbr);
  }

  public String toString() {
    return "" + n;
  }

  public int toJavaInt() {
    return (int) n;
  }

  public long toJavaLong() {
    return (long) n;
  }

  public float toJavaFloat() {
    return (float) n;
  }

  public double toJavaDouble() {
    return (double) n;
  }

  public BigInteger toJavaBigInteger() {
    return BigInteger.valueOf((long) n);
  }
}
