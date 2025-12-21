/**
 * CLYSM Printer - Object display
 *
 * Converts kernel objects to readable string representation.
 */

/**
 * Printer - converts kernel objects to strings
 */
class Printer {
  constructor(kernel) {
    this.kernel = kernel;
  }

  /**
   * Print an object to string
   */
  print(obj) {
    return this.printObject(obj);
  }

  /**
   * Print an object with internal dispatch
   */
  printObject(obj) {
    const kernel = this.kernel;

    // NIL
    if (kernel.isNull(obj)) {
      return 'NIL';
    }

    // Fixnum
    if (kernel.isFixnum(obj)) {
      return String(kernel.fixnumValue(obj));
    }

    // Character
    if (kernel.isCharacter(obj)) {
      return this.printCharacter(obj);
    }

    // String
    if (kernel.isString(obj)) {
      return this.printString(obj);
    }

    // Symbol
    if (kernel.isSymbol(obj)) {
      return this.printSymbol(obj);
    }

    // Cons / List
    if (kernel.isCons(obj)) {
      return this.printList(obj);
    }

    // Vector
    if (kernel.isVector(obj)) {
      return this.printVector(obj);
    }

    // Package
    if (kernel.isPackage(obj)) {
      return this.printPackage(obj);
    }

    // Unbound marker
    if (kernel.isUnbound(obj)) {
      return '#<UNBOUND>';
    }

    // Unknown object
    return '#<UNKNOWN>';
  }

  /**
   * Print a character
   */
  printCharacter(char) {
    const code = this.kernel.charCode(char);
    switch (code) {
      case 32: return '#\\Space';
      case 10: return '#\\Newline';
      case 13: return '#\\Return';
      case 9: return '#\\Tab';
      default:
        if (code >= 32 && code < 127) {
          return `#\\${String.fromCharCode(code)}`;
        }
        return `#\\U+${code.toString(16).toUpperCase().padStart(4, '0')}`;
    }
  }

  /**
   * Print a string (with quotes and escaping)
   */
  printString(str) {
    const js = this.kernel.stringToJS(str);
    let result = '"';
    for (const ch of js) {
      switch (ch) {
        case '"': result += '\\"'; break;
        case '\\': result += '\\\\'; break;
        case '\n': result += '\\n'; break;
        case '\t': result += '\\t'; break;
        case '\r': result += '\\r'; break;
        default: result += ch; break;
      }
    }
    result += '"';
    return result;
  }

  /**
   * Print a symbol
   */
  printSymbol(sym) {
    const kernel = this.kernel;
    const name = kernel.stringToJS(kernel.symbolName(sym));
    const pkg = kernel.symbolPackage(sym);

    // Check if it's a keyword
    if (!kernel.isNull(pkg) && kernel.eq(pkg, kernel.KEYWORD)) {
      return ':' + name;
    }

    // Check for uninterned symbol
    if (kernel.isNull(pkg)) {
      return '#:' + name;
    }

    // Regular symbol
    return name;
  }

  /**
   * Print a list (proper list, dotted pair, or improper list)
   */
  printList(cons) {
    const kernel = this.kernel;
    const elements = [];
    let current = cons;
    let dotted = false;

    // Limit to prevent infinite loops with circular structures
    const maxElements = 1000;
    let count = 0;

    while (kernel.isCons(current) && count < maxElements) {
      elements.push(this.printObject(kernel.car(current)));
      current = kernel.cdr(current);
      count++;
    }

    if (count >= maxElements) {
      elements.push('...');
    } else if (!kernel.isNull(current)) {
      // Improper list (dotted pair)
      dotted = true;
    }

    if (dotted) {
      return '(' + elements.join(' ') + ' . ' + this.printObject(current) + ')';
    }
    return '(' + elements.join(' ') + ')';
  }

  /**
   * Print a vector
   */
  printVector(vec) {
    const kernel = this.kernel;
    const len = kernel.vectorLength(vec);
    const elements = [];

    for (let i = 0; i < len && i < 1000; i++) {
      elements.push(this.printObject(kernel.svref(vec, i)));
    }

    if (len > 1000) {
      elements.push('...');
    }

    return '#(' + elements.join(' ') + ')';
  }

  /**
   * Print a package
   */
  printPackage(pkg) {
    const name = this.kernel.stringToJS(this.kernel.packageName(pkg));
    return `#<PACKAGE "${name}">`;
  }

  /**
   * Print for REPL (returns the printed value)
   */
  princ(obj) {
    // princ prints without escape characters
    if (this.kernel.isString(obj)) {
      return this.kernel.stringToJS(obj);
    }
    if (this.kernel.isCharacter(obj)) {
      return String.fromCharCode(this.kernel.charCode(obj));
    }
    return this.print(obj);
  }

  /**
   * Print with newline (for REPL)
   */
  prin1(obj) {
    return this.print(obj);
  }
}

export { Printer };
export default Printer;
