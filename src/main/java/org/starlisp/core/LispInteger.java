package org.starlisp.core;

public abstract class LispInteger extends LispNumber {
  public static LispInteger parse(String str) {
    return LispFixnum.parse(str);
  } // TODO: be smart choose proper subclass

  public abstract LispInteger mod(LispInteger n);

  public abstract LispInteger ash(LispInteger n);
}
