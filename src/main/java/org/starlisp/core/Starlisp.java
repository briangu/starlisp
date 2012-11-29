package org.starlisp.core;

/*
** TODO: * Fix up exceptions in places to be nicer somehow. Especially
**         for read-related stuffs
**       * Think about lexical scoping... dynamic scoping might be more of a PITA than I thought initially (dynamic wins on
**         ease of implementation... _but_). Lexical might not need be so difficult given passable environments, also nlambdas
**         as a method for recursion would be sort of cute in this case (or do we use the y-combinator? =p)
**       * Think about a procedure abstract class or interface, for all things having something called "apply"
**       * Try later to move away from pure list structure for exprs, instead substituting with a subclass of Procedure
**         possibly internally containing the same list structure, this is going to make lexical scoping among other things
**         much smoother (as well as removing serious amounts of clutter from eval)
**       * Fix up EOF-handling
**       * Fix up equals for LispNumbers and more
*/

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.EventObject;

public final class Starlisp {
  public static final Symbol t = intern("t");
  public static final Symbol<LispStream> standardOutput = intern("*standard-output*");
  public static final Symbol<LispStream> standardInput = intern("*standard-input*");
  public static final Symbol<LispStream> standardError = intern("*standard-error*");
  public static final Symbol lambda = intern("lambda");
  public static final Symbol quote = intern("quote");
  public static final Symbol _if = intern("if");
  public static final Symbol macro = intern("macro");
  public static final Symbol internalError = intern("internal-error");
  private static final Symbol in = intern("in");
  private static final Symbol out = intern("out");

  // KLUDGE: since we use null to represent nil (silly! it just creates lots of troubles, think before you code ;_;) this is needed everywhere and your car
  public static String toStringOrNull(LispObject obj) {
    return (obj != null) ? obj.toString() : "nil";
  }

  // Here be dragons, heavily optimized stackery
  private static final int STACK_SIZE = 32768 * 2;
  private static int stackSize = 0;
  private static LispObject[] stack = new LispObject[STACK_SIZE];

  private static final void saveEnvironment() {
    ++stackSize;
  } // Basically create a hole separating stack frames

  private static final void restoreEnvironment() {
    --stackSize;
    for (; stack[stackSize] != null; stackSize -= 2) {
      ((Symbol) stack[stackSize]).value = stack[stackSize - 1];
      stack[stackSize] = null;
      stack[stackSize - 1] = null;
    }
  }

  private static final void bind(Symbol sbl, LispObject value) { // Bind a variable to a value. Save old value on stack.
    LispObject oldValue = sbl.value;
    sbl.value = value;
    for (int i = stackSize - 1; stack[i] != null; i -= 2)
      if (stack[i] == sbl) return;      // Avoid creating buried bindings
    stack[stackSize++] = oldValue;
    stack[stackSize++] = sbl;
  }

  /* Evaluates a list of expressions and returns a freshly allocated list with the results */
  private static final Cons evlis(Cons list) {
    Cons result, last;
    if (list == null) return null;
    result = last = new Cons(evalHead(list.car()), null);
    for (Cons c = (Cons) list.cdr(); c != null; c = (Cons) c.cdr())
      last = (Cons) (last.setCdr(new Cons(evalHead(c.car()), null)));
    return result;
  }

  /* Like evlis, but returns a freshly allocated java array with the results instead */
  private static final LispObject[] evlisArray(Cons list) {
    LispObject[] res = new LispObject[(list == null) ? 0 : list.length()];
    int i = 0;
    for (Cons c = list; c != null; c = (Cons) c.cdr())
      res[i++] = evalHead(c.car());
    return res;
  }

  /* For evalling stuff not in tail position, saves and restores environment */
  private static final LispObject evalHead(LispObject obj) {
    LispObject res;
    saveEnvironment();
    try {
      res = evalTail(obj);
    } // The try-finally might have a slight (near-negligable) speed-impact but it is safer
    finally {
      restoreEnvironment();
    }
    return res;
  }

  /**
   * Evaluate code in the current dynamic environment
   */
  public static final LispObject eval(LispObject obj) {
    return evalHead(obj);
  } // To the outside world we're known as but eval

  /* The heart and blood of any interpreter, eval
The need for Tail Call Optimization, TCO, calls for some ugly hacks (which also
renders this function near unsplittable, explaining it's hugeness).
think of every obj = xxx; continue; sequence as a tail-recursive call to eval(xxx), even though
it might all look like a while loop. This function was more readable before implementing tail call optimization.
TL;DR: It doesn't look pretty, but it gets the job done....
Also: there is pseudo-lisp commentary, since all the casting makes the java-code messylicous,
but hard to avoid when implementing a dynamically typed language in a statically typed one. */
  private static final LispObject evalTail(LispObject obj) {
    while (true) {
      if (obj instanceof Symbol)
        return ((Symbol) obj).value;
      else if (obj instanceof Cons) {
        Cons list = (Cons) obj;
        if (list.car() == _if) {    // TODO: check if there is something after else-clause and explode if that is the case
          LispObject res = evalHead(((Cons) list.cdr()).car()); // (eval-head (cadr list))
          if (res != null) {
            obj = ((Cons) ((Cons) list.cdr()).cdr()).car();
            continue;
          }             // (eval-tail (caddr list))
          else if (((Cons) ((Cons) list.cdr()).cdr()).cdr() != null) {                // (cddr list)
            obj = ((Cons) ((Cons) ((Cons) list.cdr()).cdr()).cdr()).car();
            continue;
          } // (eval-tail (cadddr list))
          else return null;
        } else if (list.car() == quote)
          return ((Cons) list.cdr()).car();            // (cadr list)
        else if (list.car() == lambda || list.car() == macro)
          return list;                            // Lambdas and macros are self-quoting (Here we would also bind environment if lexical scoping)
        else { // Just brace for it, apply function to arguments
          LispObject first = evalHead(list.car());
          if (first instanceof Cons) {
            Cons f1rst = (Cons) first;           // Java's being stupid, not letting me reuse the identifier "first"
            if (f1rst.car() == lambda) {
              LispObject lambdaVar = ((Cons) f1rst.cdr()).car();  // (cadr f1rst)
              Cons lambdaBody = (Cons) ((Cons) f1rst.cdr()).cdr(); // (cddr f1rst)
              Cons argList = (Cons) list.cdr();              // (cdr list)
              if (lambdaVar != null) {                       // lambda expects variables, this is the hairy part
                // When lambdaVar instanceof Symbol we are only interested in rest-param, thus no args is ok.
                if (argList == null && lambdaVar instanceof Cons)
                  throw new LispException(internalError, "Too few args (zero in fact): " + obj);
                Cons evalledArgs = evlis(argList); // Eval the arguments
                if (lambdaVar instanceof Symbol)   // null car of varlist means we _only_ want rest-parameter
                  bind((Symbol) lambdaVar, evalledArgs);
                else
                  for (Cons c = (Cons) lambdaVar; ; c = (Cons) c.cdr()) {
                    if (c.cdr() == null) {
                      if (evalledArgs.cdr() != null)
                        throw new LispException(internalError, "Too many args: " + obj);
                      bind((Symbol) c.car(), evalledArgs.car());
                      break;
                    }
                    if (!(c.cdr() instanceof Cons)) { // rest-parameter
                      bind((Symbol) c.car(), evalledArgs.car());
                      bind((Symbol) c.cdr(), evalledArgs.cdr());
                      break;
                    }
                    bind((Symbol) c.car(), evalledArgs.car());
                    evalledArgs = (Cons)evalledArgs.cdr();
                    if (evalledArgs == null)
                      throw new LispException(internalError, "Too few args: " + obj);
                  }
              } // Phew... hairy...
              if (lambdaBody == null)
                return null;                                                        // I've no body
              for (; lambdaBody.cdr() != null; lambdaBody = (Cons) lambdaBody.cdr())
                evalHead(lambdaBody.car()); // Eval body sequentially, leave last form for TCO
              obj = lambdaBody.car();
              continue;
            } /* (eval-tail (car lambda-body)) */  /* you got all that? */ else if (f1rst.car() == macro) { // KLUDGE: kinda strange implementation of macro, huh?
              // (eval-tail (eval-head `((lambda ,@(cdr f1rst)) ',list)))
              // (eval-tail (eval-head (list (cons 'lambda (cdr f1rst)) (list 'quote list)))
              obj = evalHead(cons(cons(lambda, (Cons)f1rst.cdr()), cons(cons(quote, cons(list, null)), null)));
              continue;
            } else
              throw new LispException(internalError, "You can't just pretend lists to be functions, when they aren't: " + obj.toString());
          } else if (first instanceof Procedure)
            // (apply first (evlis-array (cdr list)))
            return ((Procedure) first).applyArgs(evlisArray((Cons) list.cdr()));
          else
            throw new LispException(internalError, "internal error: " + toStringOrNull(obj));
        }
      } else
        return obj;
    }
  }

  public static LispObject prin1(LispObject obj, LispStream stream) {
    LispStream s = (stream != null) ? stream : standardOutput.value;
    if (obj != null)
      // TODO: rewrite this using toStringOrNull instead (infact maybe get rid of the entire .printObject thing)
      s.writeJavaString(obj.toString());
    else s.writeJavaString("nil");          // Due to the funnyness of null as nil
    // s.terpri();
    return obj;
  }

  public static Cons cons(LispObject car, LispObject cdr) {
    return new Cons(car, cdr);
  }

  public static LispObject car(Cons list) {
    return (list == null) ? null : list.car();
  }

  public static LispObject cdr(Cons list) {
    return (list == null) ? null : list.cdr();
  }

  public static Symbol intern(String str) {
    return Symbol.intern(str);
  }

  public static LispObject read(LispStream stream) throws IOException {
    return ((stream != null) ? stream : standardInput.value).read();
  }

  public static LispChar readChar(LispStream stream) throws IOException {
    return new LispChar(((stream != null) ? stream : standardInput.value).readJavaChar());
  }

  public static LispChar writeChar(LispChar ch, LispStream stream) throws IOException {
    (stream != null ? stream : standardOutput.value).writeJavaChar(ch.ch);
    return ch;
  }

  public static LispObject eq(LispObject obj1, LispObject obj2) {
    return obj1 == obj2 ? t : null;
  }

  public static Cons symbols() {
    return Symbol.getSymbols();
  }

  public static LispObject symbolValue(Symbol sbl) {
    return sbl.value;
  }

  public static LispObject atom(LispObject obj) {
    return (obj instanceof Cons) ? null : t;
  }

  private static long genSymCounter = 0;

  public static LispObject gensym() {
    return new Symbol("G" + genSymCounter++);
  }

  public static LispObject eql(LispObject a, LispObject b) {
    return (a == null || b == null) ? eq(a, b) :
        !a.getClass().isInstance(b) ? null :
            (a instanceof LispChar) ? (((LispChar) a).ch == ((LispChar) a).ch) ? t : null :
                (a instanceof LispNumber) ? (((LispNumber) a).equals((LispNumber) b) ? t : null) :
                    eq(a, b);
  }

  /* Gives me everything I need to bootstrap my lisp */
  public static void initEnvironment() {
    t.value = t;

    try {
      standardOutput.value = new LispStream(null, System.out);
      standardInput.value = new LispStream(System.in, null);
      standardError.value = new LispStream(null, System.err);
    } catch (UnsupportedEncodingException e) {
    }         // Oh shut up! TODO: Major kludge this would seem like

    intern("Class").value = new JavaObject(Class.class); // All that is needed to bootstrap the Java interface.

    // Here go all SUBRs YAY! Redundancy is redundant etc. Messy is messy etc.
    intern("cons").value = new LispSubr("cons", 2) {
      public LispObject apply(LispObject[] o) {
        return cons(o[0], o[1]);
      }
    };
    intern("car").value = new LispSubr("car", 1) {
      public LispObject apply(LispObject[] o) {
        return car((Cons) o[0]);
      }
    };
    intern("cdr").value = new LispSubr("cdr", 1) {
      public LispObject apply(LispObject[] o) {
        return cdr((Cons) o[0]);
      }
    };
    intern("rplaca").value = new LispSubr("rplaca", 2) {
      public LispObject apply(LispObject[] o) {
        ((Cons) o[0]).setCar(o[1]);
        return o[0];
      }
    };
    intern("rplacd").value = new LispSubr("rplacd", 2) {
      public LispObject apply(LispObject[] o) {
        ((Cons) o[0]).setCdr((Cons)o[1]);
        return o[0];
      }
    };
    intern("prin1").value = new LispSubr("prin1", 1, 2) {
      public LispObject apply(LispObject[] o) {
        return prin1(o[0], (o.length > 1) ? (LispStream) o[1] : null);
      }
    };
    intern("eq?").value = new LispSubr("eq?", 2) {
      public LispObject apply(LispObject[] o) {
        return eq(o[0], o[1]);
      }
    };
    intern("atom?").value = new LispSubr("atom?", 1) {
      public LispObject apply(LispObject[] o) {
        return atom(o[0]);
      }
    };
    intern("set").value = new LispSubr("set", 2) {
      public LispObject apply(LispObject[] o) {
        return ((Symbol) o[0]).value = o[1];
      }
    };
    intern("eval").value = new LispSubr("eval", 1) {
      public LispObject apply(LispObject[] o) {
        return eval(o[0]);
      }
    };
    intern("symbols").value = new LispSubr("symbols") {
      public LispObject apply(LispObject[] o) {
        return symbols();
      }
    };
    intern("symbol-value").value = new LispSubr("symbol-value", 1) {
      public LispObject apply(LispObject[] o) {
        return (o[0] == null) ? null : symbolValue((Symbol) o[0]);
      }
    };
    intern("gensym").value = new LispSubr("gensym") {
      public LispObject apply(LispObject[] o) {
        return gensym();
      }
    };
    intern("intern").value = new LispSubr("intern", 1) {
      public LispObject apply(LispObject[] o) {
        if (o[0] instanceof LispString) return intern(((LispString) o[0]).toJavaString());
        if (o[0] instanceof Symbol) return ((Symbol) o[0]).intern();
        throw new LispException(internalError, "Bad argument");
      }
    };
    intern("+").value = new LispSubr("+", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).add((LispNumber) o[1]);
      }
    };
    intern("-").value = new LispSubr("-", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).sub((LispNumber) o[1]);
      }
    };
    intern("*").value = new LispSubr("*", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).mul((LispNumber) o[1]);
      }
    };
    intern("/").value = new LispSubr("/", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).div((LispNumber) o[1]);
      }
    };
    intern("mod").value = new LispSubr("mod", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispInteger) o[0]).mod((LispInteger) o[1]);
      }
    };
    intern("ash").value = new LispSubr("ash", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispInteger) o[0]).ash((LispInteger) o[1]);
      }
    };
    intern("neg?").value = new LispSubr("neg?", 1) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).negP() ? t : null;
      }
    };
    intern("eql?").value = new LispSubr("eql?", 2) {
      public LispObject apply(LispObject[] o) {
        return eql(o[0], o[1]);
      }
    };
    intern("=").value = new LispSubr("=", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispNumber) o[0]).equals((LispNumber) o[1]) ? t : null;
      }
    };
    intern("char=").value = new LispSubr("char=", 2) {
      public LispObject apply(LispObject[] o) {
        return (((LispChar) o[0]).ch == ((LispChar) o[1]).ch) ? t : null;
      }
    };
    intern("aref").value = new LispSubr("aref", 2) {
      public LispObject apply(LispObject[] o) {
        return ((LispArray) o[0]).aref(((LispInteger) o[1]).toJavaInt());
      }
    };
    intern("aset").value = new LispSubr("aset", 3) {
      public LispObject apply(LispObject[] o) {
        return ((LispArray) o[0]).aset(((LispInteger) o[1]).toJavaInt(), o[2]);
      }
    };
    intern("exit").value = new LispSubr("exit", 0, 1) {
      public LispObject apply(LispObject[] o) {
        System.exit((o.length < 1) ? 0 : ((LispNumber) o[0]).toJavaInt());
        return null;
      }
    };
    intern("get-time").value = new LispSubr("get-time") {
      public LispObject apply(LispObject[] o) {
        return new LispFixnum(System.currentTimeMillis());
      }
    };
    intern("read-char").value = new LispSubr("read-char", 0, 1) {
      public LispObject apply(LispObject[] o) {
        try {
          return readChar((o.length > 0) ? (LispStream) o[0] : null);
        } catch (IOException e) {
          throw new LispException(internalError, "An IOException just occured to me, " + this.toString());
        }
      }
    };
    intern("write-char").value = new LispSubr("write-char", 1, 2) {
      public LispObject apply(LispObject[] o) {
        try {
          return writeChar((LispChar) o[0], ((o.length > 1) ? (LispStream) o[1] : null));
        } catch (IOException e) {
          throw new LispException(internalError, "An IOException just occured to me, " + this.toString());
        }
      }
    };
    intern("read").value = new LispSubr("read", 0, 1) {
      public LispObject apply(LispObject[] o) {
        try {
          return read((o.length > 0) ? (LispStream) o[0] : null);
        } catch (IOException e) {
          throw new LispException(internalError, "An IOException just ocurred to me, " + this.toString());
        }
      }
    };
    intern("open").value = new LispSubr("open", 2) {
      public LispObject apply(LispObject[] o) {
        try {
          if (o[1] == in)
            return new LispStream(new FileReader(((LispString) o[0]).toJavaString()), null);
          if (o[1] == out)
            return new LispStream(null, new PrintWriter(new FileWriter(((LispString) o[0]).toJavaString())));
          throw new LispException(internalError, "You confused me, you want a stream out, or in?");
        } catch (IOException e) {
          throw new LispException(internalError, e);
        }
      }
    };
    intern("close").value = new LispSubr("close", 1) {
      public LispObject apply(LispObject[] o) {
        try {
          return ((LispStream) o[0]).close() ? t : null;
        } catch (IOException e) {
          throw new LispException(internalError, "An IOException just ocurred to me, " + this.toString());
        }
      }
    };
    intern("eof?").value = new LispSubr("eof?", 1) {
      public LispObject apply(LispObject[] o) {
        return ((LispStream) o[0]).eof() ? t : null;
      }
    };
    intern("make-listener").value = new LispSubr("make-listener", 1) {
      public LispObject apply(final LispObject[] o) {
        final class Listener implements ActionListener, KeyListener, MouseListener, WindowListener { // TODO: Implement more interfaces

          private void handle(EventObject e) {
            eval(cons(o[0], cons(new JavaObject(e), null)));
          }

          public void actionPerformed(ActionEvent e) {
            handle(e);
          }

          public void keyPressed(KeyEvent e) {
            handle(e);
          }

          public void keyReleased(KeyEvent e) {
            handle(e);
          }

          public void keyTyped(KeyEvent e) {
            handle(e);
          }

          public void mouseClicked(MouseEvent e) {
            handle(e);
          }

          public void mousePressed(MouseEvent e) {
            handle(e);
          }

          public void mouseReleased(MouseEvent e) {
            handle(e);
          }

          public void mouseEntered(MouseEvent e) {
            handle(e);
          }

          public void mouseExited(MouseEvent e) {
            handle(e);
          }

          public void windowActivated(WindowEvent e) {
            handle(e);
          }

          public void windowClosed(WindowEvent e) {
            handle(e);
          }

          public void windowClosing(WindowEvent e) {
            handle(e);
          }

          public void windowDeactivated(WindowEvent e) {
            handle(e);
          }

          public void windowDeiconified(WindowEvent e) {
            handle(e);
          }

          public void windowIconified(WindowEvent e) {
            handle(e);
          }

          public void windowOpened(WindowEvent e) {
            handle(e);
          }
        }
        return new JavaObject(new Listener());
      }
    };
    intern("make-runnable").value = new LispSubr("make-runnable", 1) {
      public LispObject apply(final LispObject[] o) {
        return new JavaObject(new Runnable() {
          public void run() {
            eval(cons(o[0], null));
          }
        });
      }
    };
    intern("make-string-input-stream").value = new LispSubr("make-string-input-stream", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispStream(new StringReader(((LispString) o[0]).toJavaString()), null);
      }
    }; // NOTE: copies string
    intern("make-string-output-stream").value = new LispSubr("make-string-output-stream") {
      public LispObject apply(LispObject[] o) {
        return new StringOutputStream();
      }
    };
    intern("get-output-stream-string").value = new LispSubr("get-output-stream-string", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispString(((StringOutputStream) o[0]).getOutputStreamString());
      }
    };
    intern("%try").value = new LispSubr("%try", 2) {
      public LispObject apply(LispObject[] o) {
        try {
          return eval(cons(o[0], null));
        } catch (Exception e) {
          return eval(cons(o[1], cons(new JavaObject(e), null)));
        }
      }
    };
    intern("throw").value = new LispSubr("throw", 1, 2) {
      public LispObject apply(LispObject[] o) {
        if (o.length == 2) {
          if (o[1] instanceof LispString)
            throw new LispException((Symbol) o[0], ((LispString) o[1]).toJavaString());
          else if (o[1] instanceof JavaObject)
            throw new LispException((Symbol) o[0], (Throwable) ((JavaObject) o[1]).getObj());
          else throw new LispException(internalError, "Throw threw a throw.");
        }
        if (o[0] instanceof JavaObject && ((JavaObject) o[0]).getObj() instanceof LispException)
          throw (LispException) ((JavaObject) o[0]).getObj();
        throw new LispException((Symbol) o[0]);
      }
    };
    intern("make-array").value = new LispSubr("make-array", 1) {
      public LispObject apply(LispObject[] o) {
        if (o[0] instanceof Cons) return new LispArray((Cons) o[0]);
        else if (o[0] instanceof LispInteger)
          return new LispArray(((LispInteger) o[0]).toJavaInt());
        else throw new LispException(internalError, "make-array wants an integer or a list");
      }
    };
    intern("make-string").value = new LispSubr("make-string", 2) {
      public LispObject apply(LispObject[] o) {
        return new LispString(((LispInteger) o[0]).toJavaInt(), (LispChar) o[1]);
      }
    };

    // Primitive due to the overloading on type, and the fact that I would need to export getting the length of a LispArray anyhow.
    intern("length").value = new LispSubr("length", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispFixnum((o[0] == null) ? 0 :
            (o[0] instanceof Cons) ? ((Cons) o[0]).length() :
                ((LispArray) o[0]).length());
      }
    };
    intern("equal?").value = new LispSubr("equal?", 2) {
      public LispObject apply(LispObject[] o) {
        return ((o[0] == null) ? o[1] == null : o[0].equals(o[1])) ? t : null;
      }
    };
    intern("sxhash").value = new LispSubr("sxhash", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispFixnum((o[0] == null) ? 0 : o[0].hashCode());
      }
    };

    // When called from compiled code will return true, but not here
    intern("running-compiled?").value = new LispSubr("running-compiled?") {
      public LispObject apply(LispObject[] o) {
        return null;
      }
    };

    intern("char->integer").value = new LispSubr("char->integer", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispFixnum((int) ((LispChar) o[0]).ch);
      }
    };
    intern("integer->char").value = new LispSubr("integer->char", 1) {
      public LispObject apply(LispObject[] o) {
        return new LispChar((char) ((LispInteger) o[0]).toJavaInt());
      }
    };


    // While this _could_ be implemented using the reflection api from within LJSP it is here due to shenigans and a
    // stronger reinforcing that this is _truly_ a part of the language and not it's library (which makes the
    // compiler-writing later easier in a way), especially since the reflection API stuff should be considered somewhat of
    // an extension of the base language to implement at will.
    intern("type?").value =
        new LispSubr("type?", 2) {
          Symbol number = intern("number")
              ,
              integer = intern("integer")
              ,
              fixnum = intern("fixnum")
              ,
              bignum = intern("bignum")
              ,
              flonum = intern("flonum")
              ,
              symbol = intern("symbol")
              ,
              cons = intern("cons")
              ,
              procedure = intern("procedure")
              ,
              subr = intern("subr")
              ,
              array = intern("array")
              ,
              string = intern("string")
              ,
              javaObject = intern("java-object")
              ,
              javaMethod = intern("java-method")
              ,
              exception = intern("exception")
              ,
              charmander = intern("char")
              ,
              stream = intern("stream")
              ,
              list = intern("list");

          public LispObject apply(LispObject[] o) {
            boolean woot = o[0] == number ? o[1] instanceof LispNumber :
                o[0] == integer ? o[1] instanceof LispInteger :
                    o[0] == fixnum ? o[1] instanceof LispFixnum :
                        o[0] == bignum ? o[1] instanceof LispBignum :
                            o[0] == flonum ? o[1] instanceof LispFlonum :
                                o[0] == symbol ? o[1] instanceof Symbol :
                                    o[0] == cons ? o[1] instanceof Cons :
                                        o[0] == list ? (o[1] == null || o[1] instanceof Cons) :
                                            o[0] == procedure ? o[1] instanceof Procedure : // what about lambdas?
                                                o[0] == subr ? o[1] instanceof LispSubr :
                                                    o[0] == array ? o[1] instanceof LispArray :
                                                        o[0] == string ? o[1] instanceof LispString :
                                                            o[0] == javaObject ? o[1] instanceof JavaObject :
                                                                o[0] == javaMethod ? o[1] instanceof JavaMethod :
                                                                    // o[0] == exception  ? o[1] instanceof LispException :
                                                                    o[0] == charmander ? o[1] instanceof LispChar :
                                                                        o[0] == stream ? o[1] instanceof LispStream :
                                                                            false;
            return woot ? t : null;
          }
        };
  }
}

