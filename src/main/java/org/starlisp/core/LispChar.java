package org.starlisp.core;

final class LispChar extends LispObject {
  public final char ch;

  public LispChar(char ch) {
    this.ch = ch;
  }

  public int hashCode() {
    return Character.valueOf(ch).hashCode();
  }

  public boolean equals(Object obj) {
    return (obj instanceof LispChar) ? ((LispChar) obj).ch == ch : false;
  }

  public String toString() {
    return "#\\" + ch;
  }
}
