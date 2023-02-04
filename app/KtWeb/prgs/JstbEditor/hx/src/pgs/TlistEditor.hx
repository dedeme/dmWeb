// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Str;
import dm.B64;
import dm.Opt;
import data.Tpath;
import data.Table;
import data.Field;
import I18n._;
import I18n._args;

/// Tables list page.
class TlistEditor {
  final wg: Domo;
  final list: Array<Table>;

  final newField = Q("input")
    .att("type", "text")
    .att("id", "autofocus")
    .style("width:500px")
  ;

  function new (wg: Domo, list: Array<Table>) {
    list.sort((t1, t2) -> t1.base() == t2.base()
      ? Str.compare(t1.shortPath(), t2.shortPath())
      : Str.compare(t1.base(), t2.base())
    );
    this.wg = wg;
    this.list = list;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {

    final trs = list.length == 0
      ? [
          Q("tr")
            .add(Q("td")
              .att("colspan", 3)
              .klass("frame")
              .style("text-align:center")
              .text(_("Without Tables")))
        ]
      : list.map(t -> Q("tr")
          .add(Q("td")
            .klass("border")
            .style("width:5px")
            .add(Ui.link(() -> del(t.fpath))
              .add(Ui.img("delete")
                .style("vertical-align:top"))))
          .add(Q("td")
            .klass("border")
            .style("width:5px;text-align:left")
            .add(t.error == ""
                ? Ui.link(() -> edit(t.fpath))
                  .klass("link")
                  .text(t.base())
                : Q("span")
                  .style("color:#800000")
                  .att("title", t.error)
                  .text(t.base())
              ))
          .add(Q("td")
            .klass("border")
            .style(
                "text-align:left;" +
                "color:" + (t.error == "" ? "#000000" : "#808080")
              )
            .text(t.shortPath()))
        )
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Table List")))
      .add(Q("table")
        .att("align", "center")
        .klass("border")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
              .add(Q("table")
                .att("align", "center")
                .add(Q("tr")
                  .add(Q("td")
                    .add(Ui.link(add)
                      .klass("link")
                      .add(Ui.img("add")
                        .style("vertical-align:top"))))
                  .add(Q("td")
                    .add(newField))))))
        .add(Q("tr").add(Q("td").att("colspan", 3).add(Q("hr"))))
        .adds(trs)
      )
    ;
  }

  // Control -------------------------------------------------------------------

  function add(): Void {
    final fpath = newField.getValue();
    if (fpath == "") {
      Ui.alert(_("Path is missing"));
      return;
    }

    Global.client.ssend([
      "prg" => Js.ws("JstbEditor"),
      "source" => Js.ws("TlistEditor"),
      "rq" => Js.ws("new"),
      "fpath" => Js.ws(fpath)
    ], rp -> {
      mk(wg);
    });
  }

  function del(fpath: String): Void {
    if (!Ui.confirm(_args(_("Remove table\n%0?"), [fpath]))) return;

    Global.client.ssend([
      "prg" => Js.ws("JstbEditor"),
      "source" => Js.ws("TlistEditor"),
      "rq" => Js.ws("del"),
      "fpath" => Js.ws(fpath)
    ], rp -> {
      mk(wg);
    });
  }

  function edit(fpath: String): Void {
    final tpath = new Tpath(new Table(fpath, ""), new Field([]), None);
    var hash = B64.encode(tpath.toJs().to());
    while (hash != "" && hash.charAt(hash.length - 1) == "=")
      hash = hash.substring(0, hash.length - 1);
    js.Browser.location.assign("?" + hash);
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("JstbEditor"),
      "source" => Js.ws("TlistEditor"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final list = rp["list"].ra().map(Table.fromJs);
      new TlistEditor(wg, list).show();
    });
  }
}
