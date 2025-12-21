/**
 * CLYSM Reader - S-expression parser
 *
 * Tokenizes and parses Lisp source code into kernel objects.
 */

// Token types
const TokenType = {
  LPAREN: 'LPAREN',     // (
  RPAREN: 'RPAREN',     // )
  DOT: 'DOT',           // .
  QUOTE: 'QUOTE',       // '
  BACKQUOTE: 'BACKQUOTE', // `
  COMMA: 'COMMA',       // ,
  COMMA_AT: 'COMMA_AT', // ,@
  NUMBER: 'NUMBER',     // integers
  STRING: 'STRING',     // "..."
  SYMBOL: 'SYMBOL',     // symbol names
  EOF: 'EOF',           // end of input
};

/**
 * Tokenizer - converts input string to tokens
 */
class Tokenizer {
  constructor(input) {
    this.input = input;
    this.pos = 0;
    this.line = 1;
    this.column = 1;
  }

  /**
   * Check if at end of input
   */
  isEOF() {
    return this.pos >= this.input.length;
  }

  /**
   * Peek at current character without consuming
   */
  peek() {
    if (this.isEOF()) return null;
    return this.input[this.pos];
  }

  /**
   * Peek at next character without consuming
   */
  peekNext() {
    if (this.pos + 1 >= this.input.length) return null;
    return this.input[this.pos + 1];
  }

  /**
   * Consume and return current character
   */
  advance() {
    const ch = this.input[this.pos++];
    if (ch === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return ch;
  }

  /**
   * Skip whitespace and comments
   */
  skipWhitespaceAndComments() {
    while (!this.isEOF()) {
      const ch = this.peek();
      if (/\s/.test(ch)) {
        this.advance();
      } else if (ch === ';') {
        // Line comment - skip until end of line
        while (!this.isEOF() && this.peek() !== '\n') {
          this.advance();
        }
      } else {
        break;
      }
    }
  }

  /**
   * Check if character is a delimiter
   */
  isDelimiter(ch) {
    if (ch === null) return true;
    return /[\s()'`,;"]/.test(ch);
  }

  /**
   * Check if character can start a number
   */
  isNumberStart(ch) {
    return /[0-9]/.test(ch);
  }

  /**
   * Check if character is part of a number
   */
  isNumberChar(ch) {
    return /[0-9]/.test(ch);
  }

  /**
   * Read a number token
   */
  readNumber(negative = false) {
    let value = '';
    while (!this.isEOF() && this.isNumberChar(this.peek())) {
      value += this.advance();
    }

    // Check for proper delimiter
    if (!this.isDelimiter(this.peek())) {
      // Not a pure number, treat as symbol
      return this.readSymbolRest((negative ? '-' : '') + value);
    }

    const num = parseInt(value, 10);
    return { type: TokenType.NUMBER, value: negative ? -num : num };
  }

  /**
   * Read a string token
   */
  readString() {
    this.advance(); // consume opening "
    let value = '';

    while (!this.isEOF()) {
      const ch = this.advance();
      if (ch === '"') {
        return { type: TokenType.STRING, value };
      }
      if (ch === '\\') {
        // Escape sequence
        if (this.isEOF()) {
          throw new SyntaxError('Unterminated string');
        }
        const escaped = this.advance();
        switch (escaped) {
          case 'n': value += '\n'; break;
          case 't': value += '\t'; break;
          case 'r': value += '\r'; break;
          case '\\': value += '\\'; break;
          case '"': value += '"'; break;
          default: value += escaped; break;
        }
      } else {
        value += ch;
      }
    }

    throw new SyntaxError('Unterminated string');
  }

  /**
   * Continue reading a symbol (after some initial characters)
   */
  readSymbolRest(initial) {
    let value = initial;
    while (!this.isEOF() && !this.isDelimiter(this.peek())) {
      value += this.advance();
    }
    return { type: TokenType.SYMBOL, value: value.toUpperCase() };
  }

  /**
   * Read a symbol token
   */
  readSymbol() {
    let value = '';
    while (!this.isEOF() && !this.isDelimiter(this.peek())) {
      value += this.advance();
    }
    return { type: TokenType.SYMBOL, value: value.toUpperCase() };
  }

  /**
   * Get next token
   */
  nextToken() {
    this.skipWhitespaceAndComments();

    if (this.isEOF()) {
      return { type: TokenType.EOF };
    }

    const ch = this.peek();

    // Single-character tokens
    switch (ch) {
      case '(':
        this.advance();
        return { type: TokenType.LPAREN };
      case ')':
        this.advance();
        return { type: TokenType.RPAREN };
      case "'":
        this.advance();
        return { type: TokenType.QUOTE };
      case '`':
        this.advance();
        return { type: TokenType.BACKQUOTE };
      case ',':
        this.advance();
        if (this.peek() === '@') {
          this.advance();
          return { type: TokenType.COMMA_AT };
        }
        return { type: TokenType.COMMA };
      case '"':
        return this.readString();
    }

    // Dot - could be DOT or start of symbol like .foo
    if (ch === '.') {
      this.advance();
      if (this.isDelimiter(this.peek())) {
        return { type: TokenType.DOT };
      }
      // Symbol starting with dot
      return this.readSymbolRest('.');
    }

    // Number or symbol starting with + or -
    if (ch === '+' || ch === '-') {
      this.advance();
      const next = this.peek();
      if (next !== null && this.isNumberStart(next)) {
        return this.readNumber(ch === '-');
      }
      // Symbol like + or +-foo
      return this.readSymbolRest(ch);
    }

    // Number
    if (this.isNumberStart(ch)) {
      return this.readNumber();
    }

    // Symbol (default)
    return this.readSymbol();
  }
}

/**
 * Reader - parses tokens into kernel objects
 */
class Reader {
  constructor(kernel) {
    this.kernel = kernel;
    this.tokenizer = null;
    this.currentToken = null;
  }

  /**
   * Read from a string
   */
  readFromString(input) {
    this.tokenizer = new Tokenizer(input);
    this.currentToken = this.tokenizer.nextToken();
    return this.read();
  }

  /**
   * Peek at current token
   */
  peek() {
    return this.currentToken;
  }

  /**
   * Consume current token and advance to next
   */
  advance() {
    const token = this.currentToken;
    this.currentToken = this.tokenizer.nextToken();
    return token;
  }

  /**
   * Read one S-expression
   */
  read() {
    const token = this.peek();

    switch (token.type) {
      case TokenType.EOF:
        throw new SyntaxError('Unexpected end of input');

      case TokenType.LPAREN:
        return this.readList();

      case TokenType.QUOTE:
        this.advance();
        const quoted = this.read();
        const quoteSym = this.kernel.intern('QUOTE');
        return this.kernel.list(quoteSym, quoted);

      case TokenType.BACKQUOTE:
        this.advance();
        const backquoted = this.read();
        const backquoteSym = this.kernel.intern('BACKQUOTE');
        return this.kernel.list(backquoteSym, backquoted);

      case TokenType.COMMA:
        this.advance();
        const unquoted = this.read();
        const commaSym = this.kernel.intern('COMMA');
        return this.kernel.list(commaSym, unquoted);

      case TokenType.COMMA_AT:
        this.advance();
        const spliced = this.read();
        const commaAtSym = this.kernel.intern('COMMA-AT');
        return this.kernel.list(commaAtSym, spliced);

      case TokenType.NUMBER:
      case TokenType.STRING:
      case TokenType.SYMBOL:
        return this.readAtom(this.advance());

      case TokenType.RPAREN:
        throw new SyntaxError('Unexpected )');

      case TokenType.DOT:
        throw new SyntaxError('Unexpected .');

      default:
        throw new SyntaxError(`Unknown token type: ${token.type}`);
    }
  }

  /**
   * Read a list (after consuming LPAREN)
   */
  readList() {
    this.advance(); // consume (

    // Empty list
    if (this.peek().type === TokenType.RPAREN) {
      this.advance();
      return this.kernel.NIL;
    }

    // Read list elements
    const elements = [];
    let dotted = null;

    while (this.peek().type !== TokenType.RPAREN) {
      if (this.peek().type === TokenType.EOF) {
        throw new SyntaxError('Unterminated list');
      }

      if (this.peek().type === TokenType.DOT) {
        this.advance(); // consume .
        if (elements.length === 0) {
          throw new SyntaxError('Nothing before . in list');
        }
        dotted = this.read();
        if (this.peek().type !== TokenType.RPAREN) {
          throw new SyntaxError('More than one element after . in list');
        }
        break;
      }

      elements.push(this.read());
    }

    this.advance(); // consume )

    // Build list from elements
    let result = dotted !== null ? dotted : this.kernel.NIL;
    for (let i = elements.length - 1; i >= 0; i--) {
      result = this.kernel.cons(elements[i], result);
    }

    return result;
  }

  /**
   * Read an atomic value
   */
  readAtom(token) {
    switch (token.type) {
      case TokenType.NUMBER:
        return this.kernel.makeFixnum(token.value);

      case TokenType.STRING:
        return this.kernel.stringFromJS(token.value);

      case TokenType.SYMBOL:
        // Handle special symbols
        if (token.value === 'NIL') {
          return this.kernel.NIL;
        }
        if (token.value === 'T') {
          return this.kernel.T;
        }
        // Check for keyword
        if (token.value.startsWith(':')) {
          return this.kernel.internKeyword(token.value.substring(1));
        }
        // Regular symbol
        return this.kernel.intern(token.value);

      default:
        throw new SyntaxError(`Cannot read atom of type: ${token.type}`);
    }
  }
}

export { Tokenizer, Reader, TokenType };
export default Reader;
