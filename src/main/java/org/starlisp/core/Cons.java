package org.starlisp.core;

// Cons extends Procedure since we might want a list like a function
public class Cons extends Procedure {
  public LispObject car;
  public LispObject cdr;

  public Cons(LispObject car, LispObject cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("(");
    for (Cons list = this; ; list = (Cons) list.cdr)
      if (list.cdr == null) {
        sb.append(Starlisp.toStringOrNull(list.car));
        break;
      } else if (!(list.cdr instanceof Cons)) {
        sb.append(Starlisp.toStringOrNull(list.car)).append(" . ").append(list.cdr.toString());
        break;
      } // Handle dotted lists
      else sb.append(Starlisp.toStringOrNull(list.car)).append(" ");
    sb.append(")");
    return sb.toString();
  }


  // This could eventually become problematic for bootstrapping.
  // (hard-coded dependence on ljsp.eval which will become obsolete upon bootstrapping)
  // If so remove this and make Cons extend LispObject, not Procedure
  private static final Symbol quote = Symbol.intern("quote");

  public LispObject run(LispObject[] o) {
    Cons list = null;
    for (int i = o.length - 1; i >= 0; --i)
      list = new Cons(new Cons(quote, new Cons(o[i], null)), list);
    return Starlisp.eval(new Cons(this, list));
  }

  private static final int hashCode(LispObject obj) {
    return (obj == null) ? 261835505 :          // Ensures different hashes between a proper and non-proper list
        (obj instanceof Cons) ? 1 + obj.hashCode() :
            obj.hashCode();
  }

  public int hashCode() {
    return hashCode(car) + 31 * hashCode(cdr);
  } // KLUDGE: hash implementation could be a bit better

  private static final boolean equals(LispObject a, LispObject b) {
    return (a == null) ? b == null : a.equals(b);
  }

  public boolean equals(Object obj) {
    return (obj instanceof Cons) ? equals(((Cons) obj).car, car) && equals(((Cons) obj).cdr, cdr) : false;
  }

  public int length() {
    int i = 0;
    for (Cons c = this; c != null; c = (Cons) c.cdr) ++i;
    return i;
  }
}