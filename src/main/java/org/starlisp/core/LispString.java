package org.starlisp.core;

final class LispString extends LispArray {
  public LispObject aset(int idx, LispObject obj) {
    if (!(obj instanceof LispChar))
      throw new LispException(Starlisp.internalError, "Only Char may be in a string.");
    return super.aset(idx, obj);
  }

  public LispString(String str) {
    super(str.length());
    for (int i = 0; i < ar.length; ++i) ar[i] = new LispChar(str.charAt(i));
  }

  public LispString(int length, LispChar ch) {
    super(length);
    for (int i = 0; i < length; ++i) ar[i] = ch;
  }

  public String toJavaString() {
    StringBuffer sb = new StringBuffer();
    for (LispObject o : ar) sb.append(((LispChar) o).ch);
    return sb.toString();
  }

  public String toString() {
    return '"' + toJavaString() + '"';
  }
}
