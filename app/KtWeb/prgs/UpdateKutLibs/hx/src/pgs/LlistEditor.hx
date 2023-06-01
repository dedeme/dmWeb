// Copyright 09-Feb-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Str;
import dm.Path;
import dm.B64;
import dm.It;
import data.Lpath;
import I18n._;
import I18n._args;

/// Tables list page.
class LlistEditor {
  final wg: Domo;
  final list: Array<Lpath>;

  final newField = Q("input")
    .att("type", "text")
    .att("id", "autofocus")
    .style("width:500px")
  ;

  function new (wg: Domo, list: Array<Lpath>) {
    list.sort((t1, t2) -> Str.compare(t1.id, t2.id));
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
              .text(_("Without Libraries")))
        ]
      : list.map(l -> Q("tr")
          .add(Q("td")
            .klass("border")
            .style("width:5px")
            .add(Ui.link(() -> del(l.id))
              .add(Ui.img("delete")
                .style("vertical-align:top"))))
          .add(Q("td")
            .klass("border")
            .style("width:5px;text-align:left")
            .add(l.found
                ? Ui.link(() -> edit(l))
                  .klass("link")
                  .text(l.id)
                : Q("span")
                  .style("color:#800000")
                  .text(l.id)
              ))
          .add(Q("td")
            .klass("border")
            .style(
                "text-align:left;" +
                "color:" + (l.found ? "#000000" : "#808080")
              )
            .text(l.fpath))
        )
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Libraries List")))
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
    if (fpath.charAt(0) != "/") {
      Ui.alert(_("Path is not absolute"));
      return;
    }
    if (fpath.charAt(fpath.length - 1) == "/") {
      Ui.alert(_("Path can not finish at '/'"));
      return;
    }

    final id = Path.name(fpath);
    final lpath = new Lpath(id, fpath, false);
    if (It.from(list).contains(lpath, (l1, l2) -> return l1.id == l2.id)) {
      Ui.alert(_args(_("Library name '%0' is duplicated"), [id]));
      return;
    }

    Global.client.ssend([
      "prg" => Js.ws("UpdateKutLibs"),
      "source" => Js.ws("LlistEditor"),
      "rq" => Js.ws("new"),
      "lpath" => lpath.toJs()
    ], rp -> {
      mk(wg);
    });
  }

  function del(id: String): Void {
    if (!Ui.confirm(_args(_("Remove library '%0'?"), [id]))) return;

    Global.client.ssend([
      "prg" => Js.ws("UpdateKutLibs"),
      "source" => Js.ws("LlistEditor"),
      "rq" => Js.ws("del"),
      "id" => Js.ws(id)
    ], rp -> {
      mk(wg);
    });
  }

  function edit(lpath: Lpath): Void {
    var hash = B64.encode(lpath.toJs().to());
    while (hash != "" && hash.charAt(hash.length - 1) == "=")
      hash = hash.substring(0, hash.length - 1);
    js.Browser.location.assign("?" + hash);
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("UpdateKutLibs"),
      "source" => Js.ws("LlistEditor"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final list = rp["list"].ra().map(Lpath.fromJs);
      new LlistEditor(wg, list).show();
    });
  }
}
