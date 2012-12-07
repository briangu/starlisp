package org.starlisp.core;

import java.io.IOException;
import java.io.Reader;

/**
 * Based on jdk's StreamTokenizer (a heavily stripped down version)
 *
 * @author  Brian Guarraci
 * @author  James Gosling
 * @see     java.io.StreamTokenizer#nextToken()
 * @see     java.io.StreamTokenizer#TT_EOF
 * @since   JDK1.0
 */

public class FastStreamTokenizer {

  private Reader reader = null;

  char[] cb = new char[1];

  char buf[] = new char[1024];
  int bufLimit = -1;

  /**
   * The next character to be considered by the nextToken method.  May also
   * be NEED_CHAR to indicate that a new character should be read, or SKIP_LF
   * to indicate that a new character should be read and, if it is a '\n'
   * character, it should be discarded and a second new character should be
   * read.
   */
  private int peekc = NEED_CHAR;

  private static final int NEED_CHAR = Integer.MAX_VALUE;

  private byte resetCType[] = new byte[256];
  private byte setCType[] = new byte[256];
  private byte ctype[] = setCType;

  private static final byte CT_WHITESPACE = 1;
  private static final byte CT_ALPHA = 4;
  private static final byte CT_QUOTE = 8;
  private static final byte CT_COMMENT = 16;

  public void useSExprSyntaxMode() {
    ctype = setCType;
  }

  public void useCharReadMode() {
    ctype = resetCType;
  }

  public char readChar() {
    byte[] current = ctype;
    char ch;
    try {
      useCharReadMode();
      ch = (char) nextToken();
    } catch (IOException e) {
      ch = (char)-1;
    } finally {
      ctype = current;
    }
    return ch;
  }

  /**
   * After a call to the <code>nextToken</code> method, this field
   * contains the type of the token just read. For a single character
   * token, its value is the single character, converted to an integer.
   * For a quoted string token, its value is the quote character.
   * Otherwise, its value is one of the following:
   * <ul>
   * <li><code>TT_WORD</code> indicates that the token is a word.
   * <li><code>TT_NUMBER</code> indicates that the token is a number.
   * <li><code>TT_EOL</code> indicates that the end of line has been read.
   *     The field can only have this value if the
   *     <code>eolIsSignificant</code> method has been called with the
   *     argument <code>true</code>.
   * <li><code>TT_EOF</code> indicates that the end of the input stream
   *     has been reached.
   * </ul>
   * <p>
   * The initial value of this field is -4.
   *
   * @see     java.io.StreamTokenizer#eolIsSignificant(boolean)
   * @see     java.io.StreamTokenizer#nextToken()
   * @see     java.io.StreamTokenizer#quoteChar(int)
   * @see     java.io.StreamTokenizer#TT_EOF
   * @see     java.io.StreamTokenizer#TT_EOL
   * @see     java.io.StreamTokenizer#TT_NUMBER
   * @see     java.io.StreamTokenizer#TT_WORD
   */
  public int ttype = TT_NOTHING;

  /**
   * A constant indicating that the end of the stream has been read.
   */
  public static final int TT_EOF = -1;

  /**
   * A constant indicating that a word token has been read.
   */
  public static final int TT_WORD = -3;

  /* A constant indicating that no token has been read, used for
  * initializing ttype.  FIXME This could be made public and
  * made available as the part of the API in a future release.
  */
  private static final int TT_NOTHING = -4;

  private FastStreamTokenizer() {
    useSExprSyntaxMode();
    whitespaceChars(0, ' ');
    wordChars(' '+1,255);
    ordinaryChar('(');
    ordinaryChar(')');
    ordinaryChar('\'') ;
    ordinaryChar('#');
    ordinaryChar('|');
    commentChar(';');
    quoteChar('"');
  }

  /**
   * Create a tokenizer that parses the given character stream.
   *
   * @param r  a Reader object providing the input stream.
   * @since   JDK1.1
   */
  public FastStreamTokenizer(Reader r) {
    this();
    if (r == null) {
      throw new NullPointerException();
    }
    reader = r;
  }

  /**
   * Resets this tokenizer's syntax table so that all characters are
   * "ordinary." See the <code>ordinaryChar</code> method
   * for more information on a character being ordinary.
   *
   * @see     java.io.StreamTokenizer#ordinaryChar(int)
   */
  public void resetSyntax() {
    for (int i = ctype.length; --i >= 0;)
      ctype[i] = 0;
  }

  /**
   * Specifies that all characters <i>c</i> in the range
   * <code>low&nbsp;&lt;=&nbsp;<i>c</i>&nbsp;&lt;=&nbsp;high</code>
   * are word constituents. A word token consists of a word constituent
   * followed by zero or more word constituents or number constituents.
   *
   * @param   low   the low end of the range.
   * @param   hi    the high end of the range.
   */
  public void wordChars(int low, int hi) {
    if (low < 0)
      low = 0;
    if (hi >= ctype.length)
      hi = ctype.length - 1;
    while (low <= hi)
      ctype[low++] |= CT_ALPHA;
  }

  /**
   * Specifies that all characters <i>c</i> in the range
   * <code>low&nbsp;&lt;=&nbsp;<i>c</i>&nbsp;&lt;=&nbsp;high</code>
   * are white space characters. White space characters serve only to
   * separate tokens in the input stream.
   *
   * <p>Any other attribute settings for the characters in the specified
   * range are cleared.
   *
   * @param   low   the low end of the range.
   * @param   hi    the high end of the range.
   */
  public void whitespaceChars(int low, int hi) {
    if (low < 0)
      low = 0;
    if (hi >= ctype.length)
      hi = ctype.length - 1;
    while (low <= hi)
      ctype[low++] = CT_WHITESPACE;
  }

  /**
   * Specifies that all characters <i>c</i> in the range
   * <code>low&nbsp;&lt;=&nbsp;<i>c</i>&nbsp;&lt;=&nbsp;high</code>
   * are "ordinary" in this tokenizer. See the
   * <code>ordinaryChar</code> method for more information on a
   * character being ordinary.
   *
   * @param   low   the low end of the range.
   * @param   hi    the high end of the range.
   * @see     java.io.StreamTokenizer#ordinaryChar(int)
   */
  public void ordinaryChars(int low, int hi) {
    if (low < 0)
      low = 0;
    if (hi >= ctype.length)
      hi = ctype.length - 1;
    while (low <= hi)
      ctype[low++] = 0;
  }

  /**
   * Specifies that the character argument is "ordinary"
   * in this tokenizer. It removes any special significance the
   * character has as a comment character, word component, string
   * delimiter, white space, or number character. When such a character
   * is encountered by the parser, the parser treats it as a
   * single-character token and sets <code>ttype</code> field to the
   * character value.
   *
   * <p>Making a line terminator character "ordinary" may interfere
   * with the ability of a <code>StreamTokenizer</code> to count
   * lines. The <code>lineno</code> method may no longer reflect
   * the presence of such terminator characters in its line count.
   *
   * @param   ch   the character.
   * @see     java.io.StreamTokenizer#ttype
   */
  public void ordinaryChar(int ch) {
    if (ch >= 0 && ch < ctype.length)
      ctype[ch] = 0;
  }

  /**
   * Specified that the character argument starts a single-line
   * comment. All characters from the comment character to the end of
   * the line are ignored by this stream tokenizer.
   *
   * <p>Any other attribute settings for the specified character are cleared.
   *
   * @param   ch   the character.
   */
  public void commentChar(int ch) {
    if (ch >= 0 && ch < ctype.length)
      ctype[ch] = CT_COMMENT;
  }

  /**
   * Specifies that matching pairs of this character delimit string
   * constants in this tokenizer.
   * <p>
   * When the <code>nextToken</code> method encounters a string
   * constant, the <code>ttype</code> field is set to the string
   * delimiter and the <code>sval</code> field is set to the body of
   * the string.
   * <p>
   * If a string quote character is encountered, then a string is
   * recognized, consisting of all characters after (but not including)
   * the string quote character, up to (but not including) the next
   * occurrence of that same string quote character, or a line
   * terminator, or end of file. The usual escape sequences such as
   * <code>"&#92;n"</code> and <code>"&#92;t"</code> are recognized and
   * converted to single characters as the string is parsed.
   *
   * <p>Any other attribute settings for the specified character are cleared.
   *
   * @param   ch   the character.
   * @see     java.io.StreamTokenizer#nextToken()
   * @see     java.io.StreamTokenizer#sval
   * @see     java.io.StreamTokenizer#ttype
   */
  public void quoteChar(int ch) {
    if (ch >= 0 && ch < ctype.length)
      ctype[ch] = CT_QUOTE;
  }

  /** Read the next character */
  private int read() throws IOException {
    int res = reader.read(cb, 0, 1);
    return res >= 0 ? cb[0] : res;
  }

  /**
   * Parses the next token from the input stream of this tokenizer.
   * The type of the next token is returned in the <code>ttype</code>
   * field. Additional information about the token may be in the
   * <code>nval</code> field or the <code>sval</code> field of this
   * tokenizer.
   * <p>
   * Typical clients of this
   * class first set up the syntax tables and then sit in a loop
   * calling nextToken to parse successive tokens until TT_EOF
   * is returned.
   *
   * @return     the value of the <code>ttype</code> field.
   * @exception  IOException  if an I/O error occurs.
   * @see        java.io.StreamTokenizer#nval
   * @see        java.io.StreamTokenizer#sval
   * @see        java.io.StreamTokenizer#ttype
   */
  public int nextToken() throws IOException {
    int c = peekc;
    if (c < 0 || c == NEED_CHAR) {
      c = read();
      if (c < 0)
        return ttype = TT_EOF;
    }
    ttype = c;              /* Just to be safe */

    /* Set peekc so that the next invocation of nextToken will read
    * another character unless peekc is reset in this invocation
    */
    peekc = NEED_CHAR;

    int chtype = c < 256 ? ctype[c] : CT_ALPHA;
    while ((chtype & CT_WHITESPACE) != 0) {
/*
      if (c == '\r') {
        c = read();
        if (c == '\n')
          c = read();
      } else {
        c = read();
      }
*/
      c = read();
      if (c < 0)
        return ttype = TT_EOF;
      chtype = c < 256 ? ctype[c] : CT_ALPHA;
    }

    if ((chtype & CT_ALPHA) != 0) {
      bufLimit = 0;
      do {
/*
        if (i >= buf.length) {
          buf = Arrays.copyOf(buf, buf.length * 2);
        }
*/
        buf[bufLimit++] = (char) c;
        c = read();
        chtype = c < 0 ? CT_WHITESPACE : c < 256 ? ctype[c] : CT_ALPHA;
      } while ((chtype & (CT_ALPHA)) != 0);
      peekc = c;
      return ttype = TT_WORD;
    }

    if ((chtype & CT_QUOTE) != 0) {
      ttype = c;
      bufLimit = 0;
      /* Invariants (because \Octal needs a lookahead):
      *   (i)  c contains char value
      *   (ii) d contains the lookahead
      */
      int d = read();
//      while (d >= 0 && d != ttype && d != '\n' && d != '\r') {
      while (d >= 0 && d != ttype) {
/*
        if (i >= buf.length) {
          buf = Arrays.copyOf(buf, buf.length * 2);
        }
*/
        buf[bufLimit++] = (char)d;
        d = read();
      }

      /* If we broke out of the loop because we found a matching quote
      * character then arrange to read a new character next time
      * around; otherwise, save the character.
      */
      peekc = (d == ttype) ? NEED_CHAR : d;

      return ttype;
    }

    if ((chtype & CT_COMMENT) != 0) {
      while ((c = read()) != '\n' && c != '\r' && c >= 0);
      peekc = c;
      return nextToken();
    }

    return ttype = c;
  }
}
