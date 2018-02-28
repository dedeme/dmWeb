// Copyright 17-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Tx");

{
  const reserved = "class arguments await break case catch const continue " +
    "debugger default delete do else enum eval export extends false finally " +
    "for function if implements import in instanceof interface let new " +
    "null package private protected public return static super switch this " +
    "throw true try typeof var void while with yield constructor self";

  const StCode = 0;
  const StLong = StCode + 1;
  const StQ = StLong + 1; // Quote
  let status = StCode;

  let level = 0;
  let isHead = false;
  let charQuotes = "";

  let left = "";
  let right = "";
  let lineCounter = 0;

  function isNumber(ch) {
    return ch >= "0" && ch <= "9";
  }

  function isUpper(ch) {
    return (ch >= "A" && ch <= "Z");
  }

  function isLetter(ch) {
    return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z");
  }

  function isNotId(ch) {
    return !isNumber(ch) && !isLetter(ch);
  }

  function html(s) {
    return s
      .split("&").join("&amp;")
      .split(" ").join("&nbsp;")
      .split("<").join("&lt;");
  }

  function formatN (n) {
    let r = "" + n;
    return It.range(4 - r.length).reduce(r, (seed, i) => "&nbsp;" + seed);
  }

  function newLine() {
    right += "<br>";
    left += "<a href='#' style='font-family: monospace;font-size: 12px;" +
      "background-color: rgb(215, 215, 215);color: #998866;'>" +
      formatN (++lineCounter) +
      "</a><br>";
  }

  function processCode (l) {
    const extractName = (tx) => tx.substring(0, tx.indexOf("(")).trim();

    let r = html(l);
    It.from(reserved.split(" ")).each(w => {
      let ix = r.indexOf(w);
      while (ix !== -1) {
        const ix2 = ix + w.length;
        if ((ix == 0 || isNotId(r.charAt(ix - 1))) &&
            (ix == r.length || isNotId(r.charAt(ix2)))
        ) {
          r = r.substring(0, ix) + "<span class='reserved'>" + w +
            "</span>" + r.substring(ix + w.length);
        }
        ix = r.indexOf(w, ix2 + 25);
      }
    })

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
        st = 4
        return seed + ch;
      } else if (st === 1) { // --------------------------------------- Number
        if (isNumber(ch)) {
          return seed + ch;
        } else {
          st = 4;
          if (isNotId(ch)) {
            st = 3;
          }
          return seed + "</span>" + ch;
        }
      } else if (st === 2) { // ----------------------------------- Class name
        if (isNotId(ch)) {
          st = 3;
          return seed + "</span>" + ch;
        }
        return seed + ch;
      } else { // ----------------------------------------------------- Letter
        if (isNotId(ch)) {
          st = 3;
        }
        return seed + ch;
      }
    });
    if (st === 1 || st === 2) {
      right += "</span>";
    }

    if (level === 0) {
      if (l.split(" ").join("").indexOf("=class") !== -1) {
        level = 1;
      }
    } else if (level === 1) {
      l = l.trim();
      if (l === "") {
        return;
      }

      if (l.startsWith("}")) {
        level = 0;
      } else if (l.startsWith("constructor ")) {
        left += "<span id='help:'></span>";
      } else if (l.startsWith("static ")) {
        left += "<span id='help:" +
          extractName(l.substring(7)).trim() +
          "'></span>";
      } else if (!isHead) {
        left += "<span id='help:" +
          extractName(l).trim() +
          "'></span>";
      }

      if (l.indexOf("{") === -1) {
        isHead = true;
      } else {
        isHead = false;
        level = 2;
      }
    } else {
      It.from(l.split("")).each(ch => {
        if (ch === "{") {
          ++level;
        } else if (ch === "}") {
          --level;
        }
      });
    }
  }

  function processLine (l) {
    if (status === StLong) { // --------------------------------------- StLong
      let ix = l.indexOf("*/");
      if (ix !== -1) {
        status = StCode;
        right += html(l.substring(0, ix + 2)) + "</span>";
        processLine(l.substring(ix + 2));
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
        processLine(l.substring(bix + 2));
      } else {
        status = StCode;
        right += html(l.substring(0, qix + 1)) + "</span>";
        processLine(l.substring(qix + 1));
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
        r = 2;
        pos = ix;
      }
      ix = l.indexOf("`"); // 5
      if (ix !== -1 && ix < pos) {
        r = 2;
        pos = ix;
      }

      if (r === 1) { // /*
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("*")) {
          right += "<span class='docComment'>/*";
          status = StLong;
        } else {
          right += "<span class='comment'>/*";
          status = StLong;
        }
        processLine(l);
      }else if (r === 2) { // //
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("/")) {
          right += "<span class='docComment'>//";
        } else {
          right += "<span class='docComment'>//";
        }
        right += html(l) + "</span>";
        newLine();
      } else if (r === 3) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = '"';
        right += "<span class='quote2'>\""
        processLine(l.substring(pos + 1));
      } else if (r === 4) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = "'";
        right += "<span class='quote1'>'"
        processLine(l.substring(pos + 1));
      } else if (r === 5) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = "`";
        right += "<span class='quote3'>`"
        processLine(l.substring(pos + 1));
      } else {
        processCode(l);
        newLine();
      }
    }
  }

  /**
   * @param {string} code
   * @return {void}
   */
  function process (code) {
    It.from(code.split("\n")).each(l => {
      processLine(l);
    });
  }

Tx = class {
  /**
   * @param {string} code
   * @return {string} 'code' modified to show as html
   */
  static mkCode (code) {
    process(code);
    return "<table border='0' width='100%' cellspacing='0'>" +
      "<tr><td class='prel' width='10px'>" +
      left +
      "</td><td class='prer'>" +
      right +
      "</td></tr></table>";
  }
}}


