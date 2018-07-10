// Copyright 10-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_Model");

module_Model = class {
  /** @return {string} */
  static idLink () {
    return "/*__hyperlink_target_id__*/";
  }

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
   * @param {string} text
   * @return {string}
   */
  static makeHelp (text) {
    if (text === "") {
      return "<p></p>";
    }

    let bf = "";
    let bls = 0;
    let state = 0;
    bf += "<p>";
    It.from(text.split("\n")).each(l => {
      if (l.trim () === "") {
        bf += "\n";
      } else if (state === 0) {
        if (/\s/.test(l[0])) {
          bls = module_Model.blanks(l);
          bf += "</p>\n<pre>" + module_Model.ltrim(l, bls);
          state = 1;
        } else {
          bf += l + "\n";
        }
      } else {
        if (/\s/.test(l[0])) {
          bf += "\n" + module_Model.ltrim (l, bls);
        } else {
          bf += "</pre><p>" + l + "\n";
          state = 0;
        }
      }
    });
    if (state === 0) {
      bf += "</p>";
    } else {
      bf += "</pre>";
    }

    return bf;
  }
}
