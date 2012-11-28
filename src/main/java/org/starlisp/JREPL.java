package org.starlisp;

import java.io.IOException;

import org.starlisp.core.LispStream;
import org.starlisp.core.Starlisp;

public class JREPL {
  public static void main(String args[]) {

    Starlisp.initEnvironment();
    while (true)                                        // REPL, with some wrapping...
      try {
        while (true) {
          ((LispStream) Starlisp.standardOutput.value).writeJavaString("\n>> ");
          Starlisp.prin1(Starlisp.eval(Starlisp.read(null)), null);
        }
      } catch (IOException e) {
        e.printStackTrace(((LispStream) Starlisp.standardError.value).out);
      } catch (RuntimeException e) {
        e.printStackTrace(((LispStream) Starlisp.standardError.value).out);
      }
  }
}
