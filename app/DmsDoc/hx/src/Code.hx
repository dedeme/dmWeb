// Copyright 24-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Js;
import dm.Tp;
import dm.Str;
import data.Settings;

private enum CodeProcessorStates {
  InCode;
  ShortComment;
  LongComment;
  Quote;
  Heredoc;
  At;
  Number;
  Reserved;
  Object;
}

/// Code page.
class Code {

  static final reserved = " break data elif else eval false for if import " +
    "loop mrun run sync this true try while "
  ;
  static final reserved2 = " assert clone dup expect fail nop pop puts swap " +
    "throw toStr "
  ;
  static final libs = " b64 blob cryp date file float int it js list map " +
    "math path stk str sys "
  ;
  static var state = InCode;
  static var skip = false;
  static var heredoc = "";

  static function formatN (n : Int) : String {
    var r = Std.string (n);
    return if (r.length < 5) "     ".substring(r.length) + r else r;
  }

  static function formatLine (l: String) {
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
        case '"':
          state = Quote;
          r.add ("<span class='quote1'>\"");
        case '@':
          if (ch1 == "" || (" {}[]().,:;").indexOf(ch1) != -1) {
            r.add ("<span class='at'>@</span>");
          } else {
            state = At;
            r.add ("<span class='at'>@");
          }
        case '`':
          final ltrim = l.trim();
          if (ltrim.startsWith("`") && ltrim.indexOf(" ") == -1) {
            state = Heredoc;
            heredoc = ltrim.substring(1) + "`";
            tmpBuf = new StringBuf();
            tmpBuf.add("`");
            r.add ("<span class='quote2'>`");
          }
        default:
          if (Str.isLetter(ch)) {
            state = ch.toUpperCase() == ch ? Object : Reserved;
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
      case Quote:
        if (ch == "\\" && ch1 != "") {
          r.add("\\" + ch1);
          skip = true;
        } else if (ch == '"' || ch1 == "") {
          state = InCode;
          r.add('"</span>');
        } else {
          r.add(ch);
        }
      case At:
        if (ch1 == "" || (" {}[]().,:;").indexOf(ch1) != -1) {
          state = InCode;
          r.add('</span>');
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
          else if (reserved2.indexOf (" " + tmp + " ") != -1)
            r.add("<span class='reserved2'>" + tmp + "</span>");
          else if (libs.indexOf (" " + tmp + " ") != -1)
            r.add("<span class='reserved3'>" + tmp + "</span>");
          else
            r.add(tmp);
          state = InCode;
          if (ch1 != "") r.add(ch);
        }
      case Heredoc:
        if (ch == "" || ch == "&nbsp;") {
          final tx = tmpBuf.toString();
          tmpBuf = new StringBuf();
          if (tx == heredoc) {
            heredoc = "";
            state = InCode;
            r.add("</span>");
          }
          r.add(ch);
        } else {
          tmpBuf.add(ch);
          r.add(ch);
        }
      case Object:
        if (Str.isLetterOrDigit(ch)  && ch1 != "") {
          tmpBuf.add(ch);
        } else {
          if (ch1 == "") tmpBuf.add(ch);
          r.add("<span class='object'>" + tmpBuf.toString() + "</span>");
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


  static function mkTds (line:Int, c: String): Tp<String, String> {

    final left = new StringBuf();
    final right = new StringBuf();

    var i = 0;
    for (l in c.split("\n")) {
      ++i;
      if (i == line) {
        left.add("<div id='link'></div>");
      }

      left.add(formatN(i) + "<br>");
      right.add(formatLine(l));
    }

    return new Tp(left.toString(), right.toString());
  }

  /// Constructor.
  ///   wg  : Container.
  ///   sett: Settings data.
  ///   lib : Library name.
  ///   path: Module path.
  ///   line: Line number.
  public static function mk (
    wg: Domo, sett: Settings, lib: String, path: String, line: Int
  ): Void {
    var codeWg = Q("div").text("empty");

    final mkCode = c -> {
      final tds = mkTds(line, c);
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

    final view = () -> {
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
      final link = Q("#link").e;
      if (link !=null) link.scrollIntoView(true);
    }

    switch (It.from(sett.paths).find(p -> p.lib == lib)) {
    case Some (p):
      Cts.client.send([
        "source" => Js.ws("Module"),
        "path" => Js.ws(p.path + "/" + path + ".dms")
      ], rp -> {
        final code = rp["code"].rs();
        if (code != "") {
          codeWg = mkCode(code);
        }
        view ();
      });
    case None:
      view ();
    }
  }

}
