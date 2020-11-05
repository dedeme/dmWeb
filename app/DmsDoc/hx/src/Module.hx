// Copyright 23-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Opt;
import dm.Js;
import dm.Str;
import dm.Path;
import dm.Tp;
import data.Settings;
import I18n._;

/// Module page.
class Module {

  static function format(doc: Array<String>): Domo {
    final ixParam = It.from(doc).indexf(e -> {
      if (e.startsWith("  ") && !e.startsWith("   ")) {
        final etrim = e.trim();
        if (etrim.length > 0) {
          final ix = etrim.indexOf(":");
          if (ix > 0) {
            final word = etrim.substring(0, ix).trim();
            if (word.length > 0 && word.indexOf(" ") == -1) return true;
          }
        }
      }
      return false;
    });
    final r = Q("div");
    var tx1 = "";
    var tx2 = "";
    if (ixParam > -1) {
      if (ixParam > 0) {
        tx1 = It.join(It.from(doc).take(ixParam), "\n");
      }
      tx2 = It.join(It.from(doc).drop(ixParam), "\n");
    } else {
      tx1 = doc.join("\n");
    }

    if (tx1 != "") {
      r.add(Q("table")
        .klass("border")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("pre")
              .text(tx1)))))
      ;
    }
    if (tx2 != "") {
      r.add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(Q("pre")
              .text(tx2)))))
      ;
    }

    return r;
  }

  // View ----------------------------------------------------------------------

  /// Constructor.
  ///   wg  : Container.
  ///   sett: Settings data.
  ///   lib : Library name.
  ///   path: Module path.
  public static function mk (
    wg: Domo, sett: Settings, lib: String, path: String
  ): Void {
    final title = Q("div")
      .klass("frame2")
    ;
    final index = Q("div");
    final overview = Q("div");
    final source = Q("div");
    final body = Q("div");

    function mkWgs (entries: Map<String, Tp<Array<String>, Int>>): Void {
      title.html("<b>" + path + "</b>");

      final entries2 = It.fromMap(entries)
        .filter(tp -> tp.e1 != "=Overview")
        .to()
      ;
      entries2.sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1);
      final len = entries2.length;
      final rows = Std.int((len - 1) / 5) + 1;
      final tds = It.range(rows).map(r -> It.range(5).map(e -> Q("td")).to())
        .to()
      ;
      final table = Q("table")
        .klass("main")
        .adds(tds.map(ts -> Q("tr").adds(ts)))
      ;
      It.from(entries2).eachIx((tp, i) -> {
        final row = Std.int(i / rows);
        final col = i % rows;
        tds[col][row].add(Q("a")
          .att("href", "#" + tp.e1)
          .text(tp.e1)
        );
      });
      index
        .removeAll()
        .add(table)
      ;

      final overviewDoc = entries.exists("=Overview")
        ? entries["=Overview"].e1
        : []
      ;
      overview
        .removeAll()
        .add(Q("div")
          .klass("frame")
          .html("<b>" + _("Overview") + "</b>"))
        .add(Q("br"))
        .add(format(overviewDoc))
      ;

      source
        .removeAll()
        .add(Q("div")
          .html("<b>" + _("File") + "</b>"))
        .add(Q("a")
          .att("href", "?" + lib + "@" + path + "&0")
          .text(path + ".dms"))
      ;

      body
        .removeAll()
        .adds(entries2.map(e -> {
            final id = e.e1;
            final doc = e.e2.e1;
            final line = e.e2.e2;
            return Q("p")
              .att("id", id)
              .add(Q("a")
                .att("name", id)
                .att("href", "?" + lib + "@" + path + "&" + line)
                .text(id))
              .add(Q("br"))
              .add(format(doc))
              .add(Q("hr"))
            ;
          }))
      ;
    }

    function extract (code: String): Map<String, Tp<Array<String>, Int>> {
      final r = new Map<String, Tp<Array<String>, Int>>();
      var preOverview = true;
      var keyword = "";
      var keywordLine = -1;
      var comment: Array<String> = [];
      var pars = "";
      var multiline = "";
      var nl = 0;
      for (l in code.split("\n")) {
        nl++;
        l = l.trim();

        if (multiline != "") {
          if (l.startsWith(multiline)) {
            l = l.substring(multiline.length);
            multiline = "";
          } else {
            continue;
          }
        }

        if (pars == "" && l.startsWith("///")) {
          if (keywordLine == -1) keywordLine = nl;
          comment.push(l.length > 4 ? l.substring(4) : "");
          continue;
        }

        if (keyword != "") {
          r[keyword] = new Tp(comment, keywordLine - 1);
          keyword = "";
          keywordLine = -1;
        }
        if (preOverview && comment.length > 0) {
          r["=Overview"] = new Tp(comment, 0);
          preOverview = false;
          keywordLine = -1;
        }
        comment = [];

        final par = pars.length == 0 ? "" : pars.charAt(pars.length - 1);
        if (par != "*" && l.startsWith("`") && l.indexOf(" ") == -1) {
          multiline = l.length == 1 ? "`" : l.substring(1) + "`";
          continue;
        }

        var skip = false;
        for (i in 0...l.length) {
          if (skip) {
            skip = false;
            continue;
          }
          final ch = l.charAt(i);

          final par = pars.length == 0 ? "" : pars.charAt(pars.length - 1);
          if (par == "*") {
            if (ch == "*" && i < l.length - 1 && l.charAt(i + 1) == "/") {
              pars = pars.substring(0, pars.length - 1);
              skip = true;
            }
            continue;
          }

          if (par == "\"") {
            if (ch == "\\" && i < l.length - 1 && l.charAt(i + 1) == "\"") {
              skip = true;
            } else if (ch == "\"" || i == l.length - 1) {
              pars = pars.substring(0, pars.length - 1);
            }
            continue;
          }

          if (ch == "/" && i < l.length - 1) {
            final ch2 = l.charAt(i + 1);
            if (ch2 == "/") {
              break;
            } else if ( ch2 == "*") {
              pars += "*";
              skip = true;
            }
            continue;
          }

          if (ch == "(" || ch == "{" || ch == "[" || ch == "\"") {
            pars += ch;
            continue;
          }

          if (
            (par == "(" && ch == ")") ||
            (par == "{" && ch == "}") ||
            (par == "[" && ch == "]")
          ) {
            pars = pars.substring(0, pars.length - 1);
            continue;
          }

          if (par != "") {
            continue;
          }

          if (ch == "=" && i == l.length - 1) {
            final l2 = l.substring(0, l.length - 1)
              .trim()
              .replace(" . ", "   ")
              .replace(":", " ")
              .replace(",", " ")
              .replace(";", " ")
              .replace(")", " ")
              .replace("}", " ")
              .replace("]", " ");
            final ix = l2.lastIndexOf(" ");
            keyword = ix == -1 ? l2 : l2.substring(ix + 1);
            if (keyword.indexOf(".") != -1) {
              keyword = "";
            }
          }
        }
      }

      return r;
    }

    function update () {
      var lpath = "";
      for (lp in sett.paths) {
        if (lp.lib == lib) {
          lpath = lp.path;
          break;
        }
      }
      if (lpath == "") {
        js.Browser.location.assign("?@");
        return;
      }

      lpath = Path.cat([lpath, path + ".dms"]);
      Cts.client.send([
        "source" => Js.ws("Module"),
        "id" => Js.ws(lib),
        "path" => Js.ws(lpath),
      ], rp -> {
        final code = rp["code"].rs();
        if (code == "") {
          js.Browser.location.assign("?@");
        } else {
          final entries = extract(code);
          mkWgs(entries);
        }
      });
    }

    Q("@title").text(Path.name(path) + "-" + Cts.app);
    wg
      .removeAll()
      .add(title)
      .add(Q("br"))
      .add(index)
      .add(Q("hr"))
      .add(overview)
      .add(Q("br"))
      .add(source)
      .add(Q("hr"))
      .add(Q("div").klass("frame"))
      .add(body)
      .adds(It.range(50).map(e -> Q("br")).to())
    ;
    update();
  }
}
