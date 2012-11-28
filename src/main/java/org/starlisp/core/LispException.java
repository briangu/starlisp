package org.starlisp.core;

final class LispException extends RuntimeException {
  public final Symbol tag;

  public LispException(Symbol tag) {
    this.tag = tag;
  }

  public LispException(Symbol tag, String message) {
    super(message);
    this.tag = tag;
  }

  public LispException(Symbol tag, String message, Throwable cause) {
    super(message, cause);
    this.tag = tag;
  }

  public LispException(Symbol tag, Throwable cause) {
    super(cause);
    this.tag = tag;
  }

  public String toString() {
    return "<" + tag + ">" + super.toString();
  }
}
