package org.starlisp.core;

import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;

/**
 * The <code>StreamTokenizer</code> class takes an input stream and
 * parses it into "tokens", allowing the tokens to be
 * read one at a time. The parsing process is controlled by a table
 * and a number of flags that can be set to various states. The
 * stream tokenizer can recognize identifiers, numbers, quoted
 * strings, and various comment styles.
 * <p>
 * Each byte read from the input stream is regarded as a character
 * in the range <code>'&#92;u0000'</code> through <code>'&#92;u00FF'</code>.
 * The character value is used to look up five possible attributes of
 * the character: <i>white space</i>, <i>alphabetic</i>,
 * <i>numeric</i>, <i>string quote</i>, and <i>comment character</i>.
 * Each character can have zero or more of these attributes.
 * <p>
 * In addition, an instance has four flags. These flags indicate:
 * <ul>
 * <li>Whether line terminators are to be returned as tokens or treated
 *     as white space that merely separates tokens.
 * <li>Whether C-style comments are to be recognized and skipped.
 * <li>Whether C++-style comments are to be recognized and skipped.
 * <li>Whether the characters of identifiers are converted to lowercase.
 * </ul>
 * <p>
 * A typical application first constructs an instance of this class,
 * sets up the syntax tables, and then repeatedly loops calling the
 * <code>nextToken</code> method in each iteration of the loop until
 * it returns the value <code>TT_EOF</code>.
 *
 * @author  James Gosling
 * @see     java.io.StreamTokenizer#nextToken()
 * @see     java.io.StreamTokenizer#TT_EOF
 * @since   JDK1.0
 */

public class FastStreamTokenizer {

  /* Only one of these will be non-null */
  private Reader reader = null;

  private char buf[] = new char[1024];

  /**
   * The next character to be considered by the nextToken method.  May also
   * be NEED_CHAR to indicate that a new character should be read, or SKIP_LF
   * to indicate that a new character should be read and, if it is a '\n'
   * character, it should be discarded and a second new character should be
   * read.
   */
  private int peekc = NEED_CHAR;

  private static final int NEED_CHAR = Integer.MAX_VALUE;
  private static final int SKIP_LF = Integer.MAX_VALUE - 1;

  private boolean pushedBack;

  private byte ctype[] = new byte[256];
  private static final byte CT_WHITESPACE = 1;
  private static final byte CT_ALPHA = 4;
  private static final byte CT_QUOTE = 8;
  private static final byte CT_COMMENT = 16;

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

  /**
   * If the current token is a word token, this field contains a
   * string giving the characters of the word token. When the current
   * token is a quoted string token, this field contains the body of
   * the string.
   * <p>
   * The current token is a word when the value of the
   * <code>ttype</code> field is <code>TT_WORD</code>. The current token is
   * a quoted string token when the value of the <code>ttype</code> field is
   * a quote character.
   * <p>
   * The initial value of this field is null.
   *
   * @see     java.io.StreamTokenizer#quoteChar(int)
   * @see     java.io.StreamTokenizer#TT_WORD
   * @see     java.io.StreamTokenizer#ttype
   */
  public String sval;

  public LispObject[] cval;

  /**
   * Create a tokenizer that parses the given character stream.
   *
   * @param r  a Reader object providing the input stream.
   * @since   JDK1.1
   */
  public FastStreamTokenizer(Reader r) {
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
    return reader.read();
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
    if (pushedBack) {
      pushedBack = false;
      return ttype;
    }
    byte ct[] = ctype;
    sval = null;

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

    int ctype = c < 256 ? ct[c] : CT_ALPHA;
    while ((ctype & CT_WHITESPACE) != 0) {
      if (c == '\r') {
        c = read();
        if (c == '\n')
          c = read();
      } else {
        c = read();
      }
      if (c < 0)
        return ttype = TT_EOF;
      ctype = c < 256 ? ct[c] : CT_ALPHA;
    }

    if ((ctype & CT_ALPHA) != 0) {
      int i = 0;
      do {
        if (i >= buf.length) {
          buf = Arrays.copyOf(buf, buf.length * 2);
        }
        buf[i++] = (char) c;
        c = read();
        ctype = c < 0 ? CT_WHITESPACE : c < 256 ? ct[c] : CT_ALPHA;
      } while ((ctype & (CT_ALPHA)) != 0);
      peekc = c;
      sval = String.copyValueOf(buf, 0, i);
      return ttype = TT_WORD;
    }

    if ((ctype & CT_QUOTE) != 0) {
      ttype = c;
      int i = 0;
      /* Invariants (because \Octal needs a lookahead):
      *   (i)  c contains char value
      *   (ii) d contains the lookahead
      */
      int d = read();
      while (d >= 0 && d != ttype && d != '\n' && d != '\r') {
        if (d == '\\') {
          c = read();
          int first = c;   /* To allow \377, but not \477 */
          if (c >= '0' && c <= '7') {
            c = c - '0';
            int c2 = read();
            if ('0' <= c2 && c2 <= '7') {
              c = (c << 3) + (c2 - '0');
              c2 = read();
              if ('0' <= c2 && c2 <= '7' && first <= '3') {
                c = (c << 3) + (c2 - '0');
                d = read();
              } else
                d = c2;
            } else
              d = c2;
          } else {
            switch (c) {
              case 'a':
                c = 0x7;
                break;
              case 'b':
                c = '\b';
                break;
              case 'f':
                c = 0xC;
                break;
              case 'n':
                c = '\n';
                break;
              case 'r':
                c = '\r';
                break;
              case 't':
                c = '\t';
                break;
              case 'v':
                c = 0xB;
                break;
            }
            d = read();
          }
        } else {
          c = d;
          d = read();
        }
        if (i >= buf.length) {
          buf = Arrays.copyOf(buf, buf.length * 2);
        }
        buf[i++] = (char)c;
      }

      /* If we broke out of the loop because we found a matching quote
      * character then arrange to read a new character next time
      * around; otherwise, save the character.
      */
      peekc = (d == ttype) ? NEED_CHAR : d;

      sval = String.copyValueOf(buf, 0, i);
      return ttype;
    }

    if ((ctype & CT_COMMENT) != 0) {
      while ((c = read()) != '\n' && c != '\r' && c >= 0);
      peekc = c;
      return nextToken();
    }

    return ttype = c;
  }

  /**
   * Causes the next call to the <code>nextToken</code> method of this
   * tokenizer to return the current value in the <code>ttype</code>
   * field, and not to modify the value in the <code>nval</code> or
   * <code>sval</code> field.
   *
   * @see     java.io.StreamTokenizer#nextToken()
   * @see     java.io.StreamTokenizer#nval
   * @see     java.io.StreamTokenizer#sval
   * @see     java.io.StreamTokenizer#ttype
   */
  public void pushBack() {
    if (ttype != TT_NOTHING)   /* No-op if nextToken() not called */
      pushedBack = true;
  }

  /**
   * Returns the string representation of the current stream token and
   * the line number it occurs on.
   *
   * <p>The precise string returned is unspecified, although the following
   * example can be considered typical:
   *
   * <blockquote><pre>Token['a'], line 10</pre></blockquote>
   *
   * @return  a string representation of the token
   * @see     java.io.StreamTokenizer#nval
   * @see     java.io.StreamTokenizer#sval
   * @see     java.io.StreamTokenizer#ttype
   */
  public String toString() {
    String ret;
    switch (ttype) {
      case TT_EOF:
        ret = "EOF";
        break;
      case TT_WORD:
        ret = sval;
        break;
      case TT_NOTHING:
        ret = "NOTHING";
        break;
      default: {
        /*
        * ttype is the first character of either a quoted string or
        * is an ordinary character. ttype can definitely not be less
        * than 0, since those are reserved values used in the previous
        * case statements
        */
        if (ttype < 256 &&
            ((ctype[ttype] & CT_QUOTE) != 0)) {
          ret = sval;
          break;
        }

        char s[] = new char[3];
        s[0] = s[2] = '\'';
        s[1] = (char) ttype;
        ret = new String(s);
        break;
      }
    }
    return "Token[" + ret + "]";
  }

}
