// Copyright 22-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Main from "./Main.js";  //eslint-disable-line

const $ = e => Ui.$(e);

const reserved = "asm auto break bool case char const continue default do " +
  "double else enum extern float for goto if inline int long register " +
  "frestrict return short signed sizeof static struct switch typedef " +
  "union unsigned void volatile while";
const directive = "#include #define #undef #ifdef #ifndef #error " +
  "#else #elif #pragma #endif #if";

const StCode = 0;
const StLong = StCode + 1;
const StQ = StLong + 1; // Quote
let status = StCode;

let charQuotes = "";

let left = "";
let right = "";
let lineCounter = 0;

function isNumber (ch) {
  return ch >= "0" && ch <= "9";
}

function isUpper (ch) {
  return (ch >= "A" && ch <= "Z");
}

function isLetter (ch) {
  return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z");
}

function isNotId (ch) {
  return !isNumber(ch) && !isLetter(ch);
}

function html (s) {
  return s
    .split("&").join("&amp;")
    .split(" ").join("&nbsp;")
    .split("<").join("&lt;");
}

function formatN (n) {
  const r = String(n);
  // eslint-disable-next-line
  return It.range(4 - r.length).reduce(r, (seed, i) => "&nbsp;" + seed);
}

function newLine () {
  right += "<br>";
  left += "<span style='font-family: monospace;font-size: 12px;" +
    "background-color: rgb(215, 215, 215);color: #998866;'>" +
    formatN(++lineCounter) +
    "</span><br>";
}

function processCode (prefix, l) {
  const makeLink = code => {
    let bf = "";
    for (let i = 0; i < code.length; ++i) {
      const ch = code[i];
      if (ch === "#") {
        bf += "_";
      } else if (ch > " ") {
        bf += ch;
      }
    }

    let ix = bf.indexOf("=");
    const ix2 = bf.indexOf("(");
    if (ix === -1 || (ix2 !== -1 && ix2 < ix)) {
      ix = ix2;
      if (ix !== -1) {
        if (bf.substring(ix).startsWith("(*")) {
          const ix2 = bf.indexOf("(", ix + 1);
          if (ix2 !== -1) {
            ix = ix2;
          }
        }
      }
    }
    if (ix === -1) {
      ix = bf.indexOf(";");
    }
    if (ix === -1) {
      ix = bf.length;
    }

    return bf.substring(0, ix);
  };

  let r = html(l);

  It.from(reserved.split(" ")).each(w => {
    let ix = r.indexOf(w);
    while (ix !== -1) {
      const ix2 = ix + w.length;
      if ((ix === 0 || isNotId(r.charAt(ix - 1))) &&
          (ix === r.length || isNotId(r.charAt(ix2)))
      ) {
        r = r.substring(0, ix) + "<span class='reserved'>" + w +
          "</span>" + r.substring(ix + w.length);
      }
      ix = r.indexOf(w, ix2 + 25);
    }
  });

  It.from(directive.split(" ")).each(w => {
    let ix = r.indexOf(w);
    while (ix !== -1) {
      const ix2 = ix + w.length;
      r = r.substring(0, ix) + "<span class='annotation'>" + w +
        "</span>" + r.substring(ix + w.length);
      ix = r.indexOf(w, ix2 + 27);
    }
  });

  let st = 0;
  right += It.from(r.split("")).reduce("", (seed, ch) => {
    if (st === 0 || st === 3) { // ------------------------- start or not id
      if (isNumber(ch)) {
        st = 1;
        return seed + "<span class='number'>" + ch;
      }
      if (isUpper(ch)) {
        st = 2;
        return seed + "<span class='className'>" + ch;
      }
      if (isNotId(ch)) {
        st = 3;
        return seed + ch;
      }
      st = 4;
      return seed + ch;
    }
    if (st === 1) { // ---------------------------------------------- Number
      if (isNumber(ch))
        return seed + ch;
      st = 4;
      if (isNotId(ch)) {
        st = 3;
      }
      return seed + "</span>" + ch;
    }
    if (st === 2) { // ------------------------------------------ Class name
      if (isNotId(ch)) {
        st = 3;
        return seed + "</span>" + ch;
      }
      return seed + ch;
    } // ------------------------------------------------------------ Letter
    if (isNotId(ch))
      st = 3;
    return seed + ch;
  });
  if (st === 1 || st === 2) {
    right += "</span>";
  }

  if (l.length > 0) {
    const ch = l.charAt(0);
    if (ch > " " &&
      ch !== "(" &&
      ch !== "}"
    ) {
      left += "<span id='" + prefix +
        makeLink(l.trim()) +
        "'></span>";
    }
  }
}

function processLine (prefix, l) {
  if (status === StLong) { // --------------------------------------- StLong
    const ix = l.indexOf("*/");
    if (ix !== -1) {
      status = StCode;
      right += html(l.substring(0, ix + 2)) + "</span>";
      processLine(prefix, l.substring(ix + 2));
    } else {
      right += html(l);
      newLine();
    }
  } else if (status === StQ) { // -------------------------------------- StQ
    const qix = l.indexOf(charQuotes);
    if (qix === -1) {
      right += html(l) + charQuotes + "</span>";
      newLine();
      status = StCode;
      return;
    }
    const bix = l.indexOf("\\");
    if (bix !== -1 && bix < qix) {
      right += html(l.substring(0, bix + 2));
      processLine(prefix, l.substring(bix + 2));
    } else {
      status = StCode;
      right += html(l.substring(0, qix + 1)) + "</span>";
      processLine(prefix, l.substring(qix + 1));
    }
  } else { // ------------------------------------------------------- StCode;
    if (l.trim() === "") {
      newLine();
      return;
    }
    let r = 0;
    let pos = 2000;
    let ix = l.indexOf("/*"); // 1
    if (ix !== -1) {
      r = 1;
      pos = ix;
    }
    ix = l.indexOf("//"); // 2
    if (ix !== -1 && ix < pos) {
      r = 2;
      pos = ix;
    }
    ix = l.indexOf("\""); // 3
    if (ix !== -1 && ix < pos) {
      r = 3;
      pos = ix;
    }
    ix = l.indexOf("'"); // 4
    if (ix !== -1 && ix < pos) {
      r = 4;
      pos = ix;
    }

    if (r === 1) { // /*
      processCode(prefix, l.substring(0, pos));
      l = l.substring(pos + 2);
      if (l.startsWith("*")) {
        right += "<span class='docComment'>/*";
        status = StLong;
      } else {
        right += "<span class='comment'>/*";
        status = StLong;
      }
      processLine(prefix, l);
    }else if (r === 2) { // //
      processCode(prefix, l.substring(0, pos));
      l = l.substring(pos + 2);
      if (l.startsWith("/")) {
        right += "<span class='docComment'>//";
      } else {
        right += "<span class='docComment'>//";
      }
      right += html(l) + "</span>";
      newLine();
    } else if (r === 3) {
      processCode(prefix, l.substring(0, pos));
      status = StQ;
      charQuotes = "\"";
      right += "<span class='quote2'>\"";
      processLine(prefix, l.substring(pos + 1));
    } else if (r === 4) {
      processCode(prefix, l.substring(0, pos));
      status = StQ;
      charQuotes = "'";
      right += "<span class='quote1'>'";
      processLine(prefix, l.substring(pos + 1));
    } else {
      processCode(prefix, l);
      newLine();
    }
  }
}

/**
    @param {string} prefix
    @param {string} code
    @return {void}
**/
function process (prefix, code) {
  It.from(code.split("\n")).each(l => {
    processLine(prefix, l);
  });
}


/**
    Code page.
**/
export default class Code {

  /**
      @param {!Main} main
  **/
  constructor (main) {
    this._main = main;
  }

  // View ----------------------------------------------------------------------

  /**
      @param {string} id
      @param {string} path
      @param {string} anchor
  **/
  show (id, path, anchor) {
    const prefix = anchor.startsWith("hp::") ? "hp::" : "hp:";
    const codeDiv = $("div");

    this._main.view.removeAll()
      .add($("div").html(
        "<table id='" + prefix + "' border='0'>" +
        "<tr><td class='frame'>" +
        "<a href='?" +
        id + "@" + path + "'>" +
        path + "</td></tr></table>"
      ))
      .add(codeDiv)
      .add($("div").html(
        // eslint-disable-next-line
        It.range(22).reduce("", (seed, i) => seed + "<p>&nbsp;</p>") +
        "<div style='position: fixed;bottom: 0px;right: 20px'>" +
        "<a href='#'><img border='0' src='img/up.png'></a></div>"
      ));

    const iBar = path.lastIndexOf("/");
    $("@title").text(
      (iBar === -1 ? path : path.substring(iBar + 1)) +
      "." + (prefix === "hp:" ? "h" : "c")
    );

    this.update(codeDiv, prefix, id, path, anchor);
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {!Domo} wg
      @param {string} prefix It can be 'hp:' or 'hp::'
      @param {string} id
      @param {string} path
      @param {string} anchor
      @return {!Promise}
  **/
  async update (wg, prefix, id, path, anchor) {
    const rq = {
      "page": "Code",
      "type": prefix === "hp:" ? "h" : "c",
      "id": id,
      "path": path
    };
    const /** !Object<string, string> */ rp = await Main.client.rq(rq);
    const code = rp["code"];

    process(prefix, code);
    wg.html(
      "<table border='0' width='100%' cellspacing='0'>" +
      "<tr><td class='prel' width='10px'>" +
      left +
      "</td><td class='prer'>" +
      right +
      "</td></tr></table>"
    );

    $("#" + anchor).e.scrollIntoView(true);
  }

}
