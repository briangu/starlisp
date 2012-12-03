package org.starlisp.core;

import java.io.PrintWriter;
import java.io.StringWriter;

public class StringOutputStream extends LispStreamImpl {
  private static StringWriter tmp; // Java is being utterly stupid, but i can use a temporary static variable to redeem part of it
  private final StringWriter stringWriter;

  public StringOutputStream() {
    super(null, new PrintWriter(tmp = new StringWriter())); // Couldn't assign directly to stringWriter here, nope...
    stringWriter = tmp;
  }

  public String getOutputStreamString() {
    StringBuffer sb = stringWriter.getBuffer();
    String result = sb.toString();
    sb.setLength(0);                                    // Clear characters in stream/buffer/whatever
    return result;
  }
}
