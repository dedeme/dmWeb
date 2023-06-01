// Copyright 10-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import I18n._;
import I18n._args;

class CodePg {
  static final reserved = "asm auto break bool case char const continue " +
    "default do double else enum extern float for goto if inline int long " +
    "register frestrict return short signed sizeof static struct switch " +
    "typedef union unsigned void volatile while";
  static final directive = "#include #define #undef #ifdef #ifndef #error " +
    "#else #elif #pragma #endif #if";

  static final StCode = 0;
  static final StLong = StCode + 1; // Long comment
  static final StQ = StLong + 1; // Quote

  final wg: Domo;
  final module: String;
  final path: String;
  final anchor: String;
  final code: String;
  final prefix: String;

  var left: String;
  var right: String;
  var lineCounter: Int;
  var charQuotes: String;
  var state: Int;

  function new (
    wg: Domo, module: String, path: String, anchor: String, code: String
  ) {
    this.wg = wg;
    this.module = module;
    this.path = path;
    this.anchor = anchor;
    this.code = code;

    prefix = anchor.startsWith("hp::") ? "hp::" : "hp:";
    left = "";
    right = "";
    lineCounter = 0;
    charQuotes = "";
    state = StCode;
    process(); // Gives value to 'left' and 'right'
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    Q("@title").text(Cts.appName + " - " + dm.Path.name(path) +
      (prefix == "hp::" ? ".c" : ".h"));

    wg
      .removeAll()
      .add(Q("table")
        .att("id", prefix)
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(Q("a")
              .att("href", "?" + module + "@" + path)
              .text(path)))))
      .add(Q("table")
        .style("boder:0;width:100%")
        .att("cellspacing", "0")
        .add(Q("tr")
          .add(Q("td")
            .klass("prel")
            .style("width:10px")
            .html(left))
          .add(Q("td")
            .klass("prer")
            .html(right))))
      .adds(It.range(30).map(i -> Q("p").html("&nbsp;")))
    ;
  }

  // Control -------------------------------------------------------------------

  function newLine (): Void {
    right += "<br>";
    left += "<span style='font-family: monospace;font-size: 12px;" +
      "background-color: rgb(215, 215, 215);color: #998866;'>" +
      formatN(++lineCounter) +
      "</span><br>";
  }

  function processCode (l: String): Void {
    function makeLink (code: String): String {
      var bf = "";
      for (i in 0...code.length) {
        final ch = code.charAt(i);
        if (ch == "#") {
          bf += "_";
        } else if (ch > " ") {
          bf += ch;
        }
      }

      var ix = bf.indexOf("=");
      final ix2 = bf.indexOf("(");
      if (ix == -1 || (ix2 != -1 && ix2 < ix)) {
        ix = ix2;
        if (ix != -1) {
          if (bf.substring(ix).startsWith("(*")) {
            final ix2 = bf.indexOf("(", ix + 1);
            if (ix2 != -1) {
              ix = ix2;
            }
          }
        }
      }
      if (ix == -1) {
        ix = bf.indexOf(";");
      }
      if (ix == -1) {
        ix = bf.length;
      }

      return bf.substring(0, ix);
    };

    var r = toHtml(l);

    It.from(reserved.split(" ")).each(w -> {
      var ix = r.indexOf(w);
      while (ix != -1) {
        final ix2 = ix + w.length;
        if ((ix == 0 || isNotId(r.charAt(ix - 1))) &&
            (ix == r.length || isNotId(r.charAt(ix2)))
        ) {
          r = r.substring(0, ix) + "<span class='reserved'>" + w +
            "</span>" + r.substring(ix + w.length);
          ix = r.indexOf(w, ix2 + 30);
        } else {
          ix = r.indexOf(w,ix2);
        }
      }
    });

    It.from(directive.split(" ")).each(w -> {
      var ix = r.indexOf(w);
      while (ix != -1) {
        final ix2 = ix + w.length;
        r = r.substring(0, ix) + "<span class='annotation'>" + w +
          "</span>" + r.substring(ix + w.length);
        ix = r.indexOf(w, ix2 + 32);
      }
    });

    var st = 0;
    right += It.from(r.split("")).reduce("", (seed, ch) -> {
      if (st == 0 || st == 3) { // ------------------------- start or not id
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
      if (st == 1) { // ---------------------------------------------- Number
        if (isNumber(ch))
          return seed + ch;
        st = 4;
        if (isNotId(ch)) {
          st = 3;
        }
        return seed + "</span>" + ch;
      }
      if (st == 2) { // ------------------------------------------ Class name
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
    if (st == 1 || st == 2) {
      right += "</span>";
    }

    if (l.length > 0) {
      final ch = l.charAt(0);
      if (ch > " " &&
        ch != "(" &&
        ch != "}"
      ) {
        left += "<span id='" + prefix +
          makeLink(l.trim()) +
          "'></span>";
      }
    }

  }

  function processLine (l: String): Void {
    if (state == StLong) { // ----------------------------------------- StLong
      final ix = l.indexOf("*/");
      if (ix != -1) {
        state = StCode;
        right += toHtml(l.substring(0, ix + 2)) + "</span>";
        processLine(l.substring(ix + 2));
      } else {
        right += toHtml(l);
        newLine();
      }
    } else if (state == StQ) { // ---------------------------------------- StQ
      final qix = l.indexOf(charQuotes);
      if (qix == -1) {
        right += toHtml(l) + charQuotes + "</span>";
        newLine();
        state = StCode;
        return;
      }
      final bix = l.indexOf("\\");
      if (bix != -1 && bix < qix) {
        right += toHtml(l.substring(0, bix + 2));
        processLine(l.substring(bix + 2));
      } else {
        state = StCode;
        right += toHtml(l.substring(0, qix + 1)) + "</span>";
        processLine(l.substring(qix + 1));
      }
    } else { // ------------------------------------------------------- StCode;
      if (l.trim() == "") {
        newLine();
        return;
      }
      var r = 0;
      var pos = 2000;
      var ix = l.indexOf("/*"); // 1
      if (ix != -1) {
        r = 1;
        pos = ix;
      }
      ix = l.indexOf("//"); // 2
      if (ix != -1 && ix < pos) {
        r = 2;
        pos = ix;
      }
      ix = l.indexOf("\""); // 3
      if (ix != -1 && ix < pos) {
        r = 3;
        pos = ix;
      }
      ix = l.indexOf("'"); // 4
      if (ix != -1 && ix < pos) {
        r = 4;
        pos = ix;
      }

      if (r == 1) { // /*
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("*")) {
          right += "<span class='docComment'>/*";
          state = StLong;
        } else {
          right += "<span class='comment'>/*";
          state = StLong;
        }
        processLine(l);
      }else if (r == 2) { // //
        processCode(l.substring(0, pos));
        l = l.substring(pos + 2);
        if (l.startsWith("/")) {
          right += "<span class='docComment'>//";
        } else {
          right += "<span class='docComment'>//";
        }
        right += toHtml(l) + "</span>";
        newLine();
      } else if (r == 3) { // "
        processCode(l.substring(0, pos));
        state = StQ;
        charQuotes = "\"";
        right += "<span class='quote2'>\"";
        processLine(l.substring(pos + 1));
      } else if (r == 4) { // '
        processCode(l.substring(0, pos));
        state = StQ;
        charQuotes = "'";
        right += "<span class='quote1'>'";
        processLine(l.substring(pos + 1));
      } else {
        processCode(l);
        newLine();
      }
    }
  }

  function process (): Void {
    It.from(code.split("\n")).each(l -> {
      processLine(l);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (
    wg: Domo, module: String, path: String, anchor: String
  ): Void {
    final isHeader = !anchor.startsWith("hp::");
    Cts.client.send([
      "source" => Js.ws("CodePg"),
      "rq" => Js.ws("code"),
      "module" => Js.ws(module),
      "path" => Js.ws(path),
      "isHeader" => Js.wb(isHeader)
    ], rp -> {
      if (rp["code"].isNull()) {
        final msg = isHeader
          ? _("[%0] Include file not found.")
          : _("[%0] C source file not found.")
        ;
        new MsgPg(wg, _args(msg, [path])).show();
        return;
      }
      final code = rp["code"].rs();

      new CodePg(wg, module, path, anchor, code).show();
      final tg = Q("#" + anchor).e;
      if (tg != null) tg.scrollIntoView(true);
    });
  }

  static function isNumber (ch: String): Bool {
    return ch >= "0" && ch <= "9";
  }

  static function isUpper (ch: String): Bool {
    return (ch >= "A" && ch <= "Z");
  }

  static function isLetter (ch: String): Bool {
    return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z");
  }

  static function isNotId (ch: String): Bool {
    return !isNumber(ch) && !isLetter(ch) && ch != "_";
  }

  function toHtml (s: String): String {
    return s
      .split("&").join("&amp;")
      .split(" ").join("&nbsp;")
      .split("<").join("&lt;")
    ;
  }

  function formatN (n: Int): String {
    final r = "" + n;
    return It.range(4 - r.length).reduce(r, (seed, i) -> "&nbsp;" + seed);
  }
}
