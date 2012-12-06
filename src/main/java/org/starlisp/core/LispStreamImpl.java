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
public class LispStreamImpl extends LispObject implements LispInputStream, LispOutputStream {
  public final Reader in;
  public final PrintWriter out;
  private static final Symbol readerError = Symbol$.MODULE$.intern("reader-error");
  private static final Symbol eofError = Symbol$.MODULE$.intern("eof-error");
  private Stack<Character> pushbackStack;
  private boolean open;
  private boolean eof;

  public LispStreamImpl(InputStream in, OutputStream out) throws UnsupportedEncodingException {
    this((in != null) ? new BufferedReader(new InputStreamReader(in, "UTF-8")) : null,
        (out != null) ? new PrintWriter(out, true) : null);
  }

  public LispStreamImpl(Reader in, PrintWriter out) {
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

  public void write(String str) {
    out.print(str);
    out.flush();
  } // Throws NullPointerException when not output stream FIXME?

  public void terpri() {
    out.println();
  } // Throws NullPointerException when not output stream FIXME?

  public boolean eof() {
    return eof;
  }

  public void write(char ch) throws IOException {
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

  public LispChar readChar() throws IOException {          // Throws NullPointerException when not input stream FIXME?
    return LispChar.create(readJavaChar());
  }

  public char readJavaChar() throws IOException {
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
  private Cell readList() throws IOException {
    Cell list, last;
    this.readChar();                                // Discard one character (should be '(')
    this.skipWhiteSpaceAndComments();
    char ch = this.peekJavaChar();
    if (ch == ')') { // Empty list
      this.readChar();                            // Discard ')'
      return null;
    }
    // First iteration of loop is wierd, and thus unrolled
    list = last = new Cell(this.read(), null);
    this.skipWhiteSpaceAndComments();
    ch = this.peekJavaChar();
    while (ch != ')') {
      if (ch == '.') { // Handle dotted lists, wee!
        this.readChar();                        // Discard '.'
        this.skipWhiteSpaceAndComments();
        ch = this.peekJavaChar();
        if (ch == ')')
          throw new LispException(readerError, "You now have me confuzzled, don't you want something after the dot?");
        last.setCdr(this.read());
        this.skipWhiteSpaceAndComments();
        if (this.peekJavaChar() != ')')
          throw new LispException(readerError, "You just might want to end the list with parentheses, even though you're a prick.");
        break;
      }
      last = (Cell) last.setCdr(new Cell(this.read(), null));
      this.skipWhiteSpaceAndComments();
      ch = this.peekJavaChar();
    }
    this.readChar();                                // Discard ')'
    return list;
  }

  private Symbol readQuotedSymbol() throws IOException {
    StringBuilder sb = new StringBuilder();
    char ch;
    this.readChar();                                // Discard '|'
    while ((ch = this.readJavaChar()) != '|') sb.append(ch);
    return sb.toString().equals("nil") ? null : Symbol$.MODULE$.intern(sb.toString());
  }

  private Cell readQuote() throws IOException {
    this.readChar();
    return new Cell(Symbol.quote(), new Cell(this.read(), null));
  }

  // TODO: "\n" and the likes
  private LispString readString() throws IOException {
    char ch;
    StringBuffer sb = new StringBuffer();
    this.readChar();                                // Discard '"'
    while ((ch = this.readJavaChar()) != '"') sb.append(ch);
    return new LispString(sb.toString());
  }

  /* Handle syntax starting on '#' */
  private LispObject dispatchFence() throws IOException {
    this.readChar();                                // Discard '#'
    char ch = this.readJavaChar();
    if (ch == ';') {
      this.read();
      return this.read();
    }                                 // Commment out a sexp
    else if (ch == '\\')
      return LispChar.create(this.readJavaChar());                            // Read  a character
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
        if (LispNumber.isNumber(str))          // Is a number
          return LispNumber.parse(str);
        else // Is a symbol: Funnyness since nil not separated from java null (early bad decision)
//          return str.equals("nil") ? null : Symbol$.MODULE$.intern(str);
          return Symbol$.MODULE$.intern(str);
    }
  }
}

