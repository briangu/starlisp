package org.starlisp.core;

import java.math.BigInteger;

final class LispBignum extends LispInteger {
  private BigInteger n;

  public static LispBignum parse(String str) {
    return new LispBignum(new BigInteger(str));
  }

  public LispBignum(BigInteger nbr) {
    n = nbr;
  }

  public LispBignum(long nbr) {
    n = BigInteger.valueOf(nbr);
  }

  public LispBignum(int nbr) {
    n = BigInteger.valueOf((long) nbr);
  }

  public int hashCode() {
    return n.hashCode();
  }

  public LispBignum add(LispBignum nbr) {
    return new LispBignum(n.add(nbr.n));
  }

  public LispBignum sub(LispBignum nbr) {
    return new LispBignum(n.subtract(nbr.n));
  }

  public LispBignum mul(LispBignum nbr) {
    return new LispBignum(n.multiply(nbr.n));
  }

  public LispNumber div(LispBignum nbr) {
    return new LispBignum(n.divide(nbr.n));
  } // TODO : RATIONALS

  public LispBignum mod(LispBignum nbr) {
    return new LispBignum(n.remainder(nbr.n));
  }

  public LispNumber add(LispNumber nbr) {
    return (nbr instanceof LispFlonum) ? (new LispFlonum(n.doubleValue())).add((LispFlonum) nbr) :
        (nbr instanceof LispFixnum) ? add(coerceFixnumToBignum(nbr)) :
            add((LispBignum) nbr);
  }

  public LispNumber sub(LispNumber nbr) {
    return (nbr instanceof LispFlonum) ? (new LispFlonum(n.doubleValue())).sub((LispFlonum) nbr) :
        (nbr instanceof LispFixnum) ? sub(coerceFixnumToBignum(nbr)) :
            sub((LispBignum) nbr);
  }

  public LispNumber mul(LispNumber nbr) {
    return (nbr instanceof LispFlonum) ? (new LispFlonum(n.doubleValue())).mul((LispFlonum) nbr) :
        (nbr instanceof LispFixnum) ? mul(coerceFixnumToBignum(nbr)) :
            mul((LispBignum) nbr);
  }

  public LispNumber div(LispNumber nbr) {
    return (nbr instanceof LispFlonum) ? (new LispFlonum(n.doubleValue())).div((LispFlonum) nbr) :
        (nbr instanceof LispFixnum) ? div(coerceFixnumToBignum(nbr)) :
            div((LispBignum) nbr);
  }

  public LispInteger mod(LispInteger nbr) {
    return (nbr instanceof LispFixnum) ? mod(coerceFixnumToBignum(nbr)) : mod((LispBignum) nbr);
  }

  public LispInteger ash(LispInteger nbr) {
    return new LispBignum(n.shiftLeft(nbr.toJavaInt()));
  } // TODO: Only well-defined between -2^31 and 2^31-1 inclusive

  public boolean equals(Object obj) {
    return (obj instanceof LispBignum) ? n.equals(((LispBignum) obj).n) :
        (obj instanceof LispFixnum) ? equals(coerceFixnumToBignum((LispFixnum) obj)) :
            (obj instanceof LispFlonum) ? obj.equals(this) :
                null;
  }

  public boolean negP() {
    return n.signum() == -1;
  }

  public String toString() {
    return n.toString();
  }

  public int toJavaInt() {
    return n.intValue();
  }

  public long toJavaLong() {
    return n.longValue();
  }

  public float toJavaFloat() {
    return n.floatValue();
  }

  public double toJavaDouble() {
    return n.doubleValue();
  }

  public BigInteger toJavaBigInteger() {
    return n;
  }
}
