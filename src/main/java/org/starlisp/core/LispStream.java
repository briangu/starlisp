package org.starlisp.core;

import java.io.IOException;

public interface LispStream {
  void writeJavaString(String str);
  void writeJavaChar(char ch) throws IOException;
  boolean eof();
  boolean close() throws IOException;
  LispObject read() throws IOException;
  LispChar readChar() throws IOException;
}
