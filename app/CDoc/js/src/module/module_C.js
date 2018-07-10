// Copyright 10-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_C");
goog.provide("module_CResult");

module_CResult = class {
  /**
   * @param {string} help
   * @param {string} line
   */
  constructor (help, line) {
    /** @private */
    this._help = help;
    /** @private */
    this._line = line;
  }

  /** @return {string} */
  help () {
    return this._help;
  }

  /** @return {string} */
  line () {
    return this._line;
  }
}

module_C = class {
  /**
   * @param {string} line
   * @return {number}
   */
  static blanks (line) {
    let r = 0;
    while (r < line.length && /\s/.test(line[r])) {
      ++r;
    }
    return r;
  }

  /**
   * @param {string} line
   * @param {number} blks
   * @return {string}
   */
  static ltrim (line, blks) {
    const bl = module_C.blanks(line);
    if (bl > blks) {
      return line.substring (blks);
    }
    return line.substring (bl);
  }

  /**
   * @param {!It<string>} file
   * @param {string} line
   * @return {!module_CResult}
   */
  static readLHelp (file, line) {
    const bl = module_C.blanks (line);
    let bf = "";

    let l = "";
    let ix = 0;
    while (file.hasNext ()) {
      l = module_C.ltrim (file.next (), bl);
      ix = l.indexOf ("*/");
      if (ix === -1) {
        if (l.startsWith(" * ")) {
          l = l.substring(3);
        } else if (l.startsWith(" *")) {
          l = l.substring(2);
        }
        bf += l;
      } else {
        break;
      }
    }

    while (file.hasNext ()) {
      l = file.next ();
      if (l.trim() != "") {
        break;
      }
      l = "";
    }

    return new module_CResult (bf, l);
  }

  /**
   * @param {!It<string>} file
   * @param {string} line
   * @return {!module_CResult}
   */
  static readSHelp (file, line) {
    let bf = line.trim().substring (4) + "\n";

    let l = "";
    let l2 = "";
    while (file.hasNext ()) {
      l = file.next ();
      l2 = l.trim();
      if (l2.startsWith ("/// ")) {
        bf += l2.substring(4) + "\n";
      } else if (l2.startsWith ("///")) {
        bf += l2.substring(3) + "\n";
      } else {
        break;
      }
    }

    while (l.trim() === "" && file.hasNext () ) {
      l = file.next();
    }

    return new module_CResult (bf, l);
  }
}
