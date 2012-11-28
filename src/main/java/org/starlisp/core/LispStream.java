package org.starlisp.core;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.Stack;

// FIXME: Doesn't terminate properly on EOF when reading symbols and other stuffs
/* The stream class used throughout. Can be input or output stream, optionally at the same time but points aren't synchronized so I advice against */
// THIS READER REALLY REALLY SUCKS SERIOUSLY HOW COULD I EVEN WRITE IT THIS BAD?
public class LispStream extends LispObject {
  public final Reader in;
  public final PrintWriter out;
  private static final Symbol readerError = Starlisp.intern("reader-error");
  private static final Symbol eofError = Starlisp.intern("eof-error");
  private Stack<Character> pushbackStack;
  private boolean open;
  private boolean eof;

  public LispStream(InputStream in, OutputStream out) throws UnsupportedEncodingException {
    this((in != null) ? new BufferedReader(new InputStreamReader(in, "UTF-8")) : null,
        (out != null) ? new PrintWriter(out, true) : null);
  }

  public LispStream(Reader in, PrintWriter out) {
    this.in = in;
    this.out = out;
    if (inputStreamP()) pushbackStack = new Stack<Character>();
    open = true;
    eof = false;
  }

  public boolean close() throws IOException {
    if (open) {
      if (inputStreamP()) in.close();
      if (outputStreamP()) out.close();
      open = false;
      return true;
    } else return false;
  }

  public void writeJavaString(String str) {
    out.print(str);
    out.flush();
  } // Throws NullPointerException when not output stream FIXME?

  public void terpri() {
    out.println();
  } // Throws NullPointerException when not output stream FIXME?

  public boolean eof() {
    return eof;
  }

  public void writeJavaChar(char ch) throws IOException {
    out.print(ch);
    if (ch == '\n') out.flush();
  }

  private void checkEOF() {
    if (eof) throw new LispException(eofError, "Hit EOF, don't read further or else... " + this);
  }

  private char readCheckEOF() throws IOException {
    int ch;
    checkEOF();
    if ((ch = in.read()) == -1) eof = true;
    return (char) ch;
  } // Throws NullPointerException

  public char readJavaChar() throws IOException {          // Throws NullPointerException when not input stream FIXME?
    // checkEOF();
    if (pushbackStack.empty()) return readCheckEOF();
    else return pushbackStack.pop();
  }

  public char peekJavaChar() throws IOException {           // Throws NullPointerException when not input stream FIXME?
    // checkEOF();
    if (pushbackStack.empty()) return pushbackStack.push(readCheckEOF());
    else return pushbackStack.peek();
  }

  public void unreadJavaChar(char ch) {
    pushbackStack.push(ch);
  } // Throws NullPointerException when not inputstream FIXME?

  public void skipWhiteSpaceAndComments() throws IOException {
    char tmp = readJavaChar();
    while (Character.isWhitespace(tmp) || tmp == ';') {
      if (tmp == ';') // if we find ; discard everything to, and with, newline
        while (readJavaChar() != '\n') ;
      tmp = readJavaChar();
    }
    unreadJavaChar(tmp);
  }

  public boolean inputStreamP() {
    return (in != null) ? true : false;
  }

  public boolean outputStreamP() {
    return (out != null) ? true : false;
  }

  public String toString() {
    return "#<" + super.toString() + ">";
  }

  /* Read in a list. Messy code ahead. */
  private Cons readList() throws IOException {
    Cons list, last;
    this.readJavaChar();                                // Discard one character (should be '(')
    this.skipWhiteSpaceAndComments();
    char ch = this.peekJavaChar();
    if (ch == ')') { // Empty list
      this.readJavaChar();                            // Discard ')'
      return null;
    }
    // First iteration of loop is wierd, and thus unrolled
    list = last = new Cons(this.read(), null);
    this.skipWhiteSpaceAndComments();
    ch = this.peekJavaChar();
    while (ch != ')') {
      if (ch == '.') { // Handle dotted lists, wee!
        this.readJavaChar();                        // Discard '.'
        this.skipWhiteSpaceAndComments();
        ch = this.peekJavaChar();
        if (ch == ')')
          throw new LispException(readerError, "You now have me confuzzled, don't you want something after the dot?");
        last.cdr = this.read();
        this.skipWhiteSpaceAndComments();
        if (this.peekJavaChar() != ')')
          throw new LispException(readerError, "You just might want to end the list with parentheses, even though you're a prick.");
        break;
      }
      last = (Cons) (last.cdr = new Cons(this.read(), null));
      this.skipWhiteSpaceAndComments();
      ch = this.peekJavaChar();
    }
    this.readJavaChar();                                // Discard ')'
    return list;
  }

  private Symbol readQuotedSymbol() throws IOException {
    StringBuilder sb = new StringBuilder();
    char ch;
    this.readJavaChar();                                // Discard '|'
    while ((ch = this.readJavaChar()) != '|') sb.append(ch);
    return sb.toString().equals("nil") ? null : new Symbol(sb.toString()).intern();
  }

  private Cons readQuote() throws IOException {
    this.readJavaChar();
    return new Cons(Starlisp.quote, new Cons(this.read(), null));
  }

  // TODO: "\n" and the likes
  private LispString readString() throws IOException {
    char ch;
    StringBuffer sb = new StringBuffer();
    this.readJavaChar();                                // Discard '"'
    while ((ch = this.readJavaChar()) != '"') sb.append(ch);
    return new LispString(sb.toString());
  }

  /* Handle syntax starting on '#' */
  private LispObject dispatchFence() throws IOException {
    this.readJavaChar();                                // Discard '#'
    char ch = this.readJavaChar();
    if (ch == ';') {
      this.read();
      return this.read();
    }                                 // Commment out a sexp
    else if (ch == '\\')
      return new LispChar(this.readJavaChar());                            // Read  a character
    else if (ch == '(') {
      this.unreadJavaChar('(');
      return new LispArray(this.readList());
    } // Read an array
    else throw new LispException(readerError, "Syntax Errol: dispatchFence()");
  }

  /* Read text return lisp data structures. */
  public LispObject read() throws IOException {
    if (!this.inputStreamP())
      throw new LispException(readerError, "You can't read what you can't read man, get over it.");

    this.skipWhiteSpaceAndComments();
    char ch = this.peekJavaChar();
    switch (ch) {
      case ')':
        this.readJavaChar();                        // Discard the lonely brace
        throw new LispException(readerError, "Lonely ending brace");
      case '.':
        this.readJavaChar();                        // Discard the stray .
        throw new LispException(readerError, "Stray dot");
      case '#':
        return this.dispatchFence();
      case '(':
        return this.readList();
      case '\'':
        return this.readQuote();             // Handle quote syntax. Having to type "(quote blaha)", when you can "'blaha" is such a chore.
      case '|':
        return this.readQuotedSymbol();
      case '"':
        return this.readString();
      default:                                        // An atom, (well ||-style symbols are atoms too)
        StringBuilder sb = new StringBuilder();
        for (ch = this.readJavaChar();
             !Character.isWhitespace(ch) && ch != '(' && ch != ')' && !eof;
             ch = this.readJavaChar())
          sb.append(ch);
        this.unreadJavaChar(ch);
        String str = sb.toString();
        if (LispNumber.javaStringMatchesLispNumber(str))          // Is a number
          return LispNumber.parse(str);
        else // Is a symbol: Funnyness since nil not separated from java null (early bad decision)
          return str.equals("nil") ? null : new Symbol(str).intern();
    }
  }
}

