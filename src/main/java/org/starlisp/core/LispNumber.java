package org.starlisp.core;

import java.math.BigInteger;
import java.util.regex.Pattern;

public abstract class LispNumber extends LispObject {
  public static final Pattern REGEX = Pattern.compile("^[+-]?\\d*\\.?(?:\\d+e)?\\d+$"); // The regex to match them all (numbers)

  public static final boolean isNumber(String str) {
    return REGEX.matcher(str).matches();
  }

  protected static final LispBignum coerceFixnumToBignum(LispNumber nbr) {
    return new LispBignum(((LispFixnum) nbr).toJavaLong());
  }

  protected static final LispFlonum coerceIntegerToFlonum(LispNumber nbr) {
    return new LispFlonum(((LispInteger) nbr).toJavaDouble());
  }

  /* Take that String and make a fitting LispNumber */
  public static LispNumber parse(String str) {
    try {
      return LispFixnum.parse(str);
    }              // KLUDGE: Will you dance the try-catch dance with me?
    catch (NumberFormatException e) {
      try {
        return LispBignum.parse(str);
      } catch (NumberFormatException ee) {
        return LispFlonum.parse(str);
      }
    }
  }

  public abstract LispNumber add(LispNumber n);

  public abstract LispNumber sub(LispNumber n);

  public abstract LispNumber mul(LispNumber n);

  public abstract LispNumber div(LispNumber n);

  public abstract boolean negP();

  public abstract int toJavaInt();                 // FIXME: Kind of redundant

  public abstract long toJavaLong();

  public abstract float toJavaFloat();               // FIXME: Kind of redundant

  public abstract double toJavaDouble();

  public abstract BigInteger toJavaBigInteger();
}

