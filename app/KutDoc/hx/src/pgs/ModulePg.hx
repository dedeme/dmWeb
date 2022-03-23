// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import data.Doc;
import data.DocEntry;
import I18n._;
import I18n._args;

class ModulePg {
  static final tab = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
  final wg: Domo;
  final pack: String;
  final path: String;
  final doc: Doc;

  function new (wg: Domo, pack: String, path: String, doc: Doc) {
    this.wg = wg;
    this.pack = pack;
    this.path = path;
    this.doc = doc;

    function fsort (e1: DocEntry, e2: DocEntry): Int {
      return e1.name > e2.name ? 1 : e2.name > e1.name ? -1 : 0;
    }
    doc.functions.sort(fsort);
    doc.values.sort(fsort);
  }

  // View ----------------------------------------------------------------------

  function index (): Domo {

    function block (entries: Array<DocEntry>, name: String): Array<Domo> {
      if (entries.length == 0) return [];

      final r: Array<Domo> = [];
      r.push(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .html("<i>" + name + "</i>"))
      );
      final h = Math.floor((entries.length - 1) / 3) + 1;
      for (y in 0...h) {
        r.push(Q("tr")
          .adds(It.range(3).map(x -> {
              final pos = x * h + y;
              if (pos < entries.length) {
                final e = entries[pos];
                return Q("td")
                  .add(Q("a")
                    .att("href", "#hp:" + e.name)
                    .html(tab + e.name))
                ;
              } else {
                return Q("td");
              }
            }))
        );
      }
      return r;
    }

    return Q("div")
      .add(Q("p")
        .klass("frame2")
        .html("<b>" + path + "</b>"))
      .add(Q("table")
        .klass("main")
        .adds(block(doc.values, "Values"))
        .adds(block(doc.functions, "Functions")))
    ;
  }

  function overview (): Domo {
    return Q("div")
      .add(Q("p")
        .klass("frame")
        .html("<b>" + _("Overview") + "</b>"))
      .adds(mkHelp(doc.doc))
      .add(Q("p")
        .html("<b>" + _("File") + "</b>")
        .add(Q("br"))
        .add(Q("a")
          .att("href", "?" + pack + "@" + path + "&hp:")
          .text(path + ".kut")))
      .add(Q("hr"))
    ;
  }

  function body (): Domo {

    function block(
      entries: Array<DocEntry>, name: String, isFunction = false
    ): Array<Domo> {

      function endEntry (e: DocEntry): Domo {
        var isNewLine = true;
        var bf2 = "";
        final code = e.code;
        for (i in 0...code.length) {
          final ch = code.charAt(i);
          if (isNewLine && ch != "\n") {
            if (ch <= " ") {
              bf2 += "&nbsp;";
            } else {
              bf2 += ch;
              isNewLine = false;
            }
          } else if (ch == "\n") {
            bf2 += "<br>";
            isNewLine = true;
          } else {
            bf2 += ch;
          }
        }
        return Q("div")
          .add(Q("p")
            .html("<tt>" + bf2 + "</tt>"))
          .adds(mkHelp(e.doc))
          .add(Q("hr"))
        ;
      }

      return It.from(entries).map(e ->
        Q("div")
          .add(Q("h3")
            .att("id", "hp:" + e.name)
            .add(Q("span")
              .text(name + " "))
            .add(Q("a")
              .att(
                "href",
                "?" + pack + "@" + path + "&hp:" + e.link
              ).text(e.name)))
          .add(endEntry(e))
      ).to();
    }

    return Q("div")
      .adds(block(doc.values, "Value"))
      .adds(block(doc.functions, "Function", true))
    ;
  }

  function show () {
    Q("@title").text(Cts.appName + " - " + dm.Path.name(path));

    wg
      .removeAll()
      .add(index())
      .add(overview())
      .add(Q("hr").klass("frame"))
      .add(body())
      .adds(It.range(30).map(i -> Q("p").html("&nbsp;")))
    ;

    final lc = js.Browser.location.href;
    final ix = lc.indexOf("#");
    if (ix != -1) {
      final tg = lc.substring(ix);
      if (tg != "#") {
        final e = Q(tg).e;
        if (e != null) {
          e.scrollIntoView(true);
        }
      }
    }
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, pack: String, path: String): Void {
    Cts.client.send([
      "source" => Js.ws("ModulePg"),
      "rq" => Js.ws("doc"),
      "pack" => Js.ws(pack),
      "path" => Js.ws(path)
    ], rp -> {
      if (rp["doc"].isNull()) {
        new MsgPg(wg, _args(_("[%0] Kut file not found."), [path])).show();
        return;
      }
      final doc = Doc.fromJs(rp["doc"]);
      new ModulePg(wg, pack, path, doc).show();
    });
  }

  public static function mkHelp (tx: String): Array<Domo> {
    if (tx.trim() == "") return [];

    final r: Array<Domo> = [];
    r.push(Q("table")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("white-space: nowrap")
          .add(Q("pre")
            .style("font-size: 14px;")
            .html(tx.replace("&", "&amp;").replace("<", "&lt;")))))
    );
    return r;
  }
}
