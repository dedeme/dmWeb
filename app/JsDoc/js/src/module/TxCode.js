// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_TxCode");

goog.require("module_TxMethod");

{
  const StCode = 0;
  const StLong = StCode + 1;
  const StLongC = StLong + 1;
  const StShortC = StLongC + 1;
  const StQ = StShortC + 1; // Quote
  let status = StCode;

  let level = 0;
  let charQuotes = "";

  let overview = " ";
  let helpBuffer = "";
  let isHeader = false;
  let headBuffer = "";
  let isHeadStatic = false;
  let headName = "";
  const methods = [];

  function isPrivate (tx) {
    return (" " + tx.split("\n").join(" ") + " ").indexOf(" @private ") !== -1;
  }

  function processHelp (tx) {
    let r = tx.trim()
      .split("\n ").join("\n")
      .split("&").join("&amp;")
      .split("<").join("&lt;");
    if (!r.startsWith("@")) {
      r = r.replace("\n@", "\n<hr>@");
    }
    let ix = r.indexOf("@param");
    while (ix !== -1) {
      let i = r.indexOf("}", ix);
      if (i != -1) {
        ++i;
        while (r.charAt(i) === " ") {
          ++i;
        }
        r = r.substring(0, i) + "<b>" + r.substring(i);
        let i2 = r.indexOf("\n", i);
        if (i2 === -1) {
          let i3 = r.indexOf(" ", i);
          if (i3 === -1) {
            r = r + "</b>";
          } else {
            r = r.substring(0, i3) + "</b>" + r.substring(i3);
          }
        } else {
          let i3 = r.indexOf(" ", i);
          if (i3 !== -1 && i3 < i2) {
            i2 = i3;
          }
          r = r.substring(0, i2) + "</b>" + r.substring(i2);
        }
      }
      ix = r.indexOf("\n@param", ix + 7);
    }
    return r;
  }

  function processCode (l) {
    const extractName = (tx) => tx.substring(0, tx.indexOf("(")).trim();
    if (isHeader) {
      const ix = l.indexOf("{");
      if (ix === -1) {
        headBuffer += "<br>&nbsp;&nbsp;&nbsp;&nbsp;" + l.trim();
      } else {
        methods.push(new module_TxMethod(
            isHeadStatic,
            headName,
            headBuffer += "<br>&nbsp;&nbsp;&nbsp;&nbsp;" +
              l.substring(0, ix).trim(),
            processHelp(helpBuffer)
          ));
        helpBuffer = "";
        level = 2;
        isHeader = false;
      }
      return
    }

    if (level === 0) {
      if (l.split(" ").join("").indexOf("=class") !== -1) {
        overview = processHelp(helpBuffer);
        level = 1;
      }
    } else if (level === 1) {
      l = l.trim();
      if (l === "") {
        return;
      }

      const help = processHelp(helpBuffer);
      const isPublic = !isPrivate(help);
      if (l.startsWith("}")) {
        level = 0;
      } else if (l.startsWith("constructor ")) {
        if (isPublic) {
          const ix = l.indexOf("{");
          if (ix === -1) {
            isHeader = true;
            isHeadStatic = false;
            headName = "";
            headBuffer = l.trim();
            return;
          } else {
            methods.push(new module_TxMethod(
              false,
              "",
              l.substring(0, ix).trim(),
              processHelp(helpBuffer)
            ));
          }
        }
      } else if (l.startsWith("static ")) {
        if (isPublic) {
          const ix = l.indexOf("{");
          if (ix === -1) {
            isHeader = true;
            isHeadStatic = true;
            headName = extractName(l.substring(7)).trim();
            headBuffer = l.trim();
            return;
          } else {
            methods.push(new module_TxMethod(
              true,
              extractName(l.substring(7)).trim(),
              l.substring(0, ix).trim(),
              processHelp(helpBuffer)
            ));
          }
        }
      } else {
        if (isPublic) {
          const ix = l.indexOf("{");
          if (ix === -1) {
            isHeader = true;
            isHeadStatic = false;
            headName = extractName(l).trim();
            headBuffer = l.trim();
            return;
          } else {
            methods.push(new module_TxMethod(
              false,
              extractName(l).trim(),
              l.substring(0, ix).trim(),
              processHelp(helpBuffer)
            ));
          }
        }
      }
      helpBuffer = "";
      level = 2;
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
        processLine(l.substring(ix + 2));
      }
    } else if (status === StLongC) { // ------------------------------ StLongC
      l = l.trim();
      let ix = l.indexOf("*/");
      if (ix !== -1) {
        helpBuffer += l.startsWith("*")
          ? (l.startsWith("*/") ? "" : l.substring(1, ix))
          : l.substring(0, ix);
        if (overview === " ") {
          overview = processHelp(helpBuffer);
        }
        status = StCode;
        processLine(l.substring(ix + 2));
      } else {
        helpBuffer += (l.startsWith("*") ? l.substring(1) : l) + "\n";
      }
    } else if (status === StShortC) { // ---------------------------- StShortC
      const ltrim = l.trim();
      if (ltrim.startsWith("///")) {
        helpBuffer += ltrim.substring(3);
      } else {
        if (overview === " ") {
          overview = processHelp(helpBuffer);
        }
        helpBuffer = "";
        status = StCode;
      }
    } else if (status === StQ) { // -------------------------------------- StQ
      const qix = l.indexOf(charQuotes);
      if (qix === -1) {
        status = StCode;
        return;
      }
      const bix = l.indexOf("\\");
      if (bix !== -1 && bix < qix) {
        processLine(l.substring(bix + 2));
      } else {
        status = StCode;
        processLine(l.substring(qix + 1));
      }
    } else { // ------------------------------------------------------- StCode
      if (l.trim() === "") {
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
        helpBuffer = "";
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("*")) {
          status = StLongC;
        } else {
          status = StLong;
        }
        processLine(l);
      }else if (r === 2) { // //
        helpBuffer = "";
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("/")) {
          helpBuffer = l.substring(1) + "\n";
          status = StShortC;
        }
      } else if (r === 3) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = '"';
        processLine(l.substring(pos + 1));
      } else if (r === 4) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = "'";
        processLine(l.substring(pos + 1));
      } else if (r === 5) {
        processCode(l.substring(0, pos));
        status = StQ;
        charQuotes = "`";
        processLine(l.substring(pos + 1));
      } else {
        processCode(l);
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

module_TxCode = class {

  /** @return {string} */
  static overview () {
    return overview;
  }

  /**
   * @param {string} code
   * @return {!Array<!module_TxMethod>} Methdos of 'code'
   */
  static mkModule (code) {
    process(code);
    return methods;
  }
}}


