// Copyright 24-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Js;
import dm.Tp;
import dm.Str;
import cm.data.Paths;

// Private ---------------------------------------------------------------------

private enum CodeProcessorStates {
  InCode;
  ShortComment;
  LongComment;
  Quote1;
  Quote2;
  Number;
  Reserved;
  Klass;
}

// Public ----------------------------------------------------------------------

/// Code page.
class Code {
  static final reserved = " break callback case cast catch class continue " +
    "default do dynamic else enum extends extern false final for function if " +
    "implements import in inline interface macro never new null override "+
    "package private public return static super switch this throw trace true " +
    "try typedef untyped using var while "
  ;

  final wg: Domo;
  final lib: String;
  final path: String;
  final line: Int;
  final code: String;

  public function new (
    wg: Domo, lib: String, path: String, line: Int, code: String
  ): Void {
    this.wg = wg;
    this.lib = lib;
    this.path = path;
    this.line = line;
    this.code = code;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final codeWg = code == ""
      ? Q("div").text("empty")
      : mkCode()
    ;
    wg
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .html('<a href="?${lib}@${path}">${path}</a>'))))
      .add(codeWg)
      .adds(It.range(25).to().map(i -> Q("p").html("&nbsp;")))
    ;
    Q("#link").e.scrollIntoView(true);
  }

  function mkCode (): Domo {
    final tds = mkTds();
    return Q("table")
      .add(Q("tr")
        .add(Q("td")
          .klass("prel")
          .style("width: 5px")
          .html(tds.e1))
        .add(Q("td")
          .klass("prer")
          .html(tds.e2)))
    ;
  }

  function mkTds (): Tp<String, String> {
    final left = new StringBuf();
    final right = new StringBuf();

    var i = 0;
    for (l in code.split("\n")) {
      ++i;
      if (i == line) {
        left.add("<div id='link'></div>");
      }

      left.add(formatN(i) + "<br>");
      right.add(formatLine(l));
    }

    return new Tp(left.toString(), right.toString());
  }

  // static --------------------------------------------------------------------

  /// Constructor.
  ///   wg   : Container.
  ///   paths: Paths list.
  ///   lib  : Library name.
  ///   path : Module path.
  ///   line : Line number.
  public static function mk (
    wg: Domo, paths: Paths, lib: String, path: String, line: Int
  ): Void {
    switch (It.from(paths.list).find(p -> p.lib == lib)) {
    case Some (p):
      Cts.client.send([
        "source" => Js.ws("Module"),
        "path" => Js.ws(p.path + "/" + path + ".hx")
      ], rp -> {
        final code = rp["code"].rs();
        new Code(wg, lib, path, line, code);
      });
    case None:
      new Code(wg, lib, path, line, "");
    }
  }

  static function formatN (n : Int) : String {
    var r = Std.string (n);
    return if (r.length < 5) "     ".substring(r.length) + r else r;
  }

  static function formatLine (l: String) {
    var state = InCode;
    var skip = false;

    final r = new StringBuf();
    var tmpBuf = new StringBuf();
    l = Str.html(l);
    final len1 = l.length - 1;
    var ch = "";
    var ch1 = "";
    for (i in 0...l.length) {
      if (skip) {
        skip = false;
        continue;
      }

      ch = l.charAt(i);
      ch1 = if (i < len1) ch1 = l.charAt (i + 1) else ch1 = "";
      if (ch == " ") {
        ch = "&nbsp;";
      }

      switch (state) {
      case InCode:
        switch (ch) {
        case "/":
          switch(ch1) {
          case "/":
            state = ShortComment;
            if (l.substring(i, i + 3) == "///")
              r.add ("<span class='docComment'>//");
            else
              r.add ("<span class='comment'>//");
            skip = true;
          case "*":
            state = LongComment;
            if (l.substring(i, i + 3) == "/**")
              r.add ("<span class='docComment'>/*");
            else
              r.add ("<span class='comment'>/*");
            skip = true;
          default:
            r.add(ch);
          }
        case "'":
          state = Quote1;
          r.add ("<span class='quote1'>'");
        case '"':
          state = Quote2;
          r.add ("<span class='quote2'>\"");
        default:
          if (Str.isLetter(ch)) {
            state = ch.toUpperCase() == ch ? Klass : Reserved;
            tmpBuf = new StringBuf();
            tmpBuf.add(ch);
          } else if (Str.isDigit(ch)) {
            state = Number;
            r.add("<span class='number'>" + ch);
          } else {
            r.add(ch);
          }
        }
      case ShortComment:
        if (ch1 == "") {
          state = InCode;
          r.add(ch);
          r.add("</span>");
        } else {
          r.add(ch);
        }
      case LongComment:
        if (ch == "*" && ch1 == "/") {
          state = InCode;
          r.add("*/</span>");
          skip = true;
        } else {
          r.add(ch);
        }
      case Quote1:
        if (ch == "\\" && ch1 != "") {
          r.add("\\" + ch1);
          skip = true;
        } else if (ch == "'" || ch1 == "") {
          state = InCode;
          r.add("'</span>");
        } else {
          r.add(ch);
        }
      case Quote2:
        if (ch == "\\" && ch1 != "") {
          r.add("\\" + ch1);
          skip = true;
        } else if (ch == '"' || ch1 == "") {
          state = InCode;
          r.add('"</span>');
        } else {
          r.add(ch);
        }
      case Reserved:
        if (Str.isLetterOrDigit(ch) && ch1 != "") {
          tmpBuf.add(ch);
        } else {
          if (ch1 == "") tmpBuf.add(ch);
          final tmp = tmpBuf.toString();
          if (reserved.indexOf (" " + tmp + " ") != -1)
            r.add("<span class='reserved'>" + tmp + "</span>");
          else
            r.add(tmp);
          state = InCode;
          if (ch1 != "") r.add(ch);
        }
      case Klass:
        if (Str.isLetterOrDigit(ch)  && ch1 != "") {
          tmpBuf.add(ch);
        } else {
          if (ch1 == "") tmpBuf.add(ch);
          r.add("<span class='className'>" + tmpBuf.toString() + "</span>");
          state = InCode;
          if (ch1 != "") r.add(ch);
        }

      case Number:
        if ((!Str.isDigit (ch) && ch != ".") || ch1 == "") {
          state = InCode;
          r.add("</span>");
        }
        r.add(ch);
      }
    }
    r.add("<br>");

    return r.toString();
  }

}
