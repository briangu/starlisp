package org.starlisp.core;

import java.util.Arrays;

class LispArray extends LispObject {
  protected final LispObject[] ar;

  public LispArray(int length) {
    ar = new LispObject[length];
  }

  public LispArray(Cons list) {
    this(list.length());
    if (length() == 0) return;
    int i = 0;
    for (Cons c = list; c != null; c = (Cons) c.cdr) ar[i++] = c.car;
  }

  public LispArray(LispObject[] ar) {
    this.ar = ar;
  }

  public int hashCode() {
    return Arrays.deepHashCode(ar);
  }

  public boolean equals(Object obj) {
    return (obj instanceof LispArray) ? Arrays.deepEquals(ar, ((LispArray) obj).ar) : false;
  }

  public int length() {
    return ar.length;
  }

  public LispObject aref(int idx) {
    return ar[idx];
  }

  public LispObject aset(int idx, LispObject obj) {
    LispObject res = ar[idx];
    ar[idx] = obj;
    return res;
  }

  public String toString() {
    StringBuffer sb = new StringBuffer();
    sb.append("#(");
    for (LispObject o : ar) {
      sb.append(Starlisp.toStringOrNull(o));
      sb.append(' ');
    }
    sb.setLength(sb.length() - 1);
    sb.append(')');
    return sb.toString();
  }
}
