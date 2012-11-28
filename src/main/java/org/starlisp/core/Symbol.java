package org.starlisp.core;

public class Symbol extends LispObject {
  private static Cons symbols = null;

  public static Cons getSymbols() {
    return symbols;
  }

  public static Symbol findSymbol(String str) {
    return findSymbol(str, symbols);
  }

  private static Symbol findSymbol(String str, Cons list) { // FIXME: Make a loop out of this recursion
    if (list == null) return null;
    else if (str.equals(((Symbol) list.car).str)) return (Symbol) list.car;
    else return findSymbol(str, (Cons) list.cdr);
  }

  public LispObject value;                                // Value field, manipulated directly most of the time
  private String str;
  private boolean interned;

  public Symbol(String str) {
    this.str = str;
    this.interned = false;
  }

  public Symbol intern() {
    if (this.interned) return this;
    Symbol sbl;
    if ((sbl = findSymbol(this.str)) == null) {
      symbols = new Cons(this, symbols);
      this.interned = true;
      return this;
    } else {
      return sbl;
    }
  }

  public static Symbol intern(String str) {
    return (new Symbol(str)).intern();
  }

  public String getStr() {
    return this.str;
  }

  public String toString() {
    return this.interned ? this.str : "#:" + this.str;
  }
}
