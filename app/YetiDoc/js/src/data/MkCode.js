// Copyright 04-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

let i = 0;
const CODE = i++;
const COMMENT = i++;
const QUOTES1 = i++;
const QUOTES2 = i++;

const reserved = " as case of class data data instance default deriving do " +
  "forall foreign hiding if then else import infix infixl infixr let in " +
  " module newtype proc qualified rec type where "
;

function isId (c) {
  return (c >= "A" && c <= "Z") ||
    (c >= "a" && c <= "z") ||
    (c >= "0" && c <= "9") ||
    c === "'" || c === "_"
  ;
}

class Rs {
  /**
      @param {number} st
      @param {string} tx
  **/
  constructor (st, tx) {
    this._st = st;
    this._tx = tx;
  }

  /**
      @return {number}
  **/
  get state () {
    return this._st;
  }

  /**
      @return {string}
  **/
  get text () {
    return this._tx;
  }
}

export default class MkCode {
  /**
      @param {string} tx
      @return {string}
  **/
  static run (tx) {
    let rs = new Rs(CODE, "");
    return tx.split("\n").map(l => {
      rs = MkCode.process("", rs.state, l);
      return rs.text;
    }).join("\n");
  }

  /**
      @private
      @param {string} r
      @param {number} st is COMMENT
      @param {string} l
      @return {!Rs}
  **/
  static comment (r, st, l) {
    const ix = l.indexOf("-}");
    if (ix === -1) {
      return new Rs(st, r + l);
    }

    return MkCode.process(
      r + l.substring(0, ix + 2) + "</span>", CODE, l.substring(ix + 2));
  }

  /**
      @private
      @param {string} r
      @param {number} st is CODE
      @param {string} l (include first character)
      @return {!Rs}
  **/
  static idLow (r, st, l) {
    let i = 0;
    for (; i < l.length; ++i)
      if (!isId(l.charAt(i))) break;

    const id = l.substring(0, i);
    if (reserved.indexOf(" " + id + " ") !== -1)
      return MkCode.process(
        r + "<span class='reserved'>" + id + "</span>", st, l.substring(i)
      );
    return MkCode.process(r + id, st, l.substring(i));
  }

  /**
      @private
      @param {string} r
      @param {number} st is CODE
      @param {string} l (include first character)
      @return {!Rs}
  **/
  static idUp (r, st, l) {
    let i = 0;
    for (; i < l.length; ++i)
      if (!isId(l.charAt(i))) break;

    return MkCode.process(
      r + "<span class='className'>" + l.substring(0, i) + "</span>",
      st,
      l.substring(i)
    );
  }

  /**
      @private
      @param {string} r
      @param {number} st is CODE
      @param {string} l (include first character)
      @return {!Rs}
  **/
  static number (r, st, l) {
    function nIsN (ch) { return (ch < "0" || ch > "9") && ch !== "." }

    let i = 0;
    for (; i < l.length; ++i)
      if (nIsN(l.charAt(i))) break;

    return MkCode.process(
      r + "<span class='number'>" + l.substring(0, i) + "</span>",
      st,
      l.substring(i)
    );
  }

  /**
      @private
      @param {string} q
      @param {string} r
      @param {number} st is QUOTES
      @param {string} l (NOT include first quote)
      @return {!Rs}
  **/
  static quotes (q, r, st, l) {
    const ib = l.indexOf("\\");
    const iq = l.indexOf(q);
    if ((iq === -1 && ib !== -1) || (iq !== -1 && ib !== -1 && ib < iq)) {
      if (ib === l.length - 1)
        return new Rs(st, r + l);

      if (l.charAt(ib + 1) === q)
        return MkCode.process(
          r + l.substring(0, ib + 2), st, l.substring(ib + 2)
        );

      return MkCode.process(
        r + l.substring(0, ib + 1), st, l.substring(ib + 1)
      );
    }

    const ix = (iq === -1) ? l.length - 1 : iq;
    return MkCode.process(
      r + l.substring(0, ix + 1) + "</span>", CODE, l.substring(ix + 1)
    );
  }

  /**
      @private
      @param {string} r
      @param {number} st is CODE
      @param {string} l
      @return {!Rs}
  **/
  static code (r, st, l) {
    if (l.startsWith("--")) {
      const c = r === "" && l.startsWith("---") ? "docComment" : "comment";
      return new Rs(CODE, r + "<span class='" + c + "'>" + l + "</span>");
    }

    if (l.startsWith("{-"))
      return MkCode.process(
        r + "<span class='comment'>{-", COMMENT, l.substring(2)
      );

    const ch = l.charAt(0);

    if ((ch >= "a" && ch <= "z") || ch === "_") return MkCode.idLow(r, st, l);
    if (ch >= "A" && ch <= "Z") return MkCode.idUp(r, st, l);
    if (ch >= "0" && ch <= "9") return MkCode.number(r, st, l);

    if (ch === "'") return MkCode.quotes(
      "'", r + "<span class='quote1'>'", QUOTES1, l.substring(1)
    );
    if (ch === "\"") return MkCode.quotes(
      "\"", r + "<span class='quote2'>\"", QUOTES2, l.substring(1)
    );

    return MkCode.process(r + l.charAt(0), CODE, l.substring(1));
  }

  /**
      @private
      @param {string} r
      @param {number} st
      @param {string} l
      @return {!Rs}
  **/
  static process (r, st, l) {
    if (l === "") return new Rs(st, r);

    switch (st) {
    case COMMENT: return MkCode.comment(r, st, l);
    case QUOTES1: return MkCode.quotes("'", r, st, l);
    case QUOTES2: return MkCode.quotes("\"", r, st, l);
    default: return MkCode.code(r, st, l);
    }
  }

}
