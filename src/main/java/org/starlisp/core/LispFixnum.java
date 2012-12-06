package org.starlisp.core;

import java.math.BigInteger;

public final class LispFixnum extends LispInteger {
  private final long n;

  // An experiment with caching of low value fixnums
  static LispFixnum[] cache = new LispFixnum[65536];

  static {
    for (int i = 0; i < 65536; i++) {
      cache[i] = new LispFixnum(i);
    }
  }

  public static LispFixnum parse(String str) {
    return new LispFixnum(Long.parseLong(str));
  }

  public static LispFixnum create(long n) {
    return (n < 65536) ? cache[(int)n] : new LispFixnum(n);
  }

  public LispFixnum(long nbr) {
    n = nbr;
  }

  public int hashCode() {
    return Long.valueOf(n).hashCode();
  }

  public LispInteger add(LispFixnum nbr) {
    LispFixnum res = LispFixnum.create(n + nbr.n);
    if (((this.n ^ res.n) & (nbr.n ^ res.n)) < 0)                  // Check overflow
      return (new LispBignum(n)).add(new LispBignum(nbr.n)); // Redo addition with bignums and return
    return res;
  }

  public LispInteger sub(LispFixnum nbr) {
    LispFixnum res = LispFixnum.create(n - nbr.n);
    if (((this.n ^ res.n) & (-nbr.n ^ res.n)) < 0)          // Check overflow
      return (new LispBignum(n)).sub(new LispBignum(nbr.n));
    return res;
  }

  public LispInteger mul(LispFixnum nbr) {
    // If nlz(x) + nlz(~x) + nlz(y) + nlz(~y) < 65 multiplication _might_ overflow
    if (Long.numberOfLeadingZeros(Math.abs(n)) + Long.numberOfLeadingZeros(Math.abs(nbr.n)) < 65)
      return (new LispBignum(n)).mul(new LispBignum(nbr.n));
    return LispFixnum.create(n * nbr.n);
  }

  public LispNumber div(LispFixnum nbr) {
    return LispFixnum.create(n / nbr.n);
  } // TODO: RATIONAAAALS? (and overflow for that matter)

  public LispInteger mod(LispFixnum nbr) {
    return LispFixnum.create(n % nbr.n);
  } // Can impossibly overflow?

  public LispInteger ash(LispFixnum nbr) {
    return LispFixnum.create((nbr.n > 0) ? n << nbr.n : n >> -nbr.n);
  }     // TODO: overflow left

  public LispNumber add(LispNumber nbr) {
    return (nbr instanceof LispBignum) ? (new LispBignum(n)).add((LispBignum) nbr) :
        (nbr instanceof LispFlonum) ? (new LispFlonum((double) n)).add((LispFlonum) nbr) :
            add((LispFixnum) nbr);
  }

  public LispNumber sub(LispNumber nbr) {
    return (nbr instanceof LispBignum) ? (new LispBignum(n)).sub((LispBignum) nbr) :
        (nbr instanceof LispFlonum) ? (new LispFlonum((double) n)).sub((LispFlonum) nbr) :
            sub((LispFixnum) nbr);
  }

  public LispNumber mul(LispNumber nbr) {
    return (nbr instanceof LispBignum) ? (new LispBignum(n)).mul((LispBignum) nbr) :
        (nbr instanceof LispFlonum) ? (new LispFlonum((double) n).mul((LispFlonum) nbr)) :
            mul((LispFixnum) nbr);
  }

  public LispNumber div(LispNumber nbr) {
    return (nbr instanceof LispBignum) ? (new LispBignum(n)).div((LispBignum) nbr) :
        (nbr instanceof LispFlonum) ? (new LispFlonum((double) n)).div((LispFlonum) nbr) :
            div((LispFixnum) nbr);
  }

  public LispInteger mod(LispInteger nbr) {
    return (nbr instanceof LispBignum)
        ? (new LispBignum(n)).mod((LispBignum) nbr)
        : mod((LispFixnum) nbr);
  }

  public LispInteger ash(LispInteger nbr) {
    return ash((LispFixnum) nbr);
  }

  public boolean equals(Object obj) {
    return (obj instanceof LispFixnum) ? n == ((LispFixnum) obj).n :
        (obj instanceof LispNumber) ? obj.equals(this) :
            false;
  }

  public boolean negP() {
    return n < 0;
  }

  public String toString() {
    return String.valueOf(n);
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
    return BigInteger.valueOf(n);
  }
}
