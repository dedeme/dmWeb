// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import dm.Menu;
import cm.data.ModelFloats;
import I18n._;

/// Models page.
class ModelsPg {
  final wg: Domo;
  final dataGroups: Array<ModelFloats>;
  final typeSel: String;

  function new (wg: Domo, typeSel: String, dataGroups: Array<ModelFloats>) {
    this.wg = wg;
    this.typeSel = typeSel;
    for (vs in dataGroups)
      vs.values.sort((e1, e2) -> e1.value < e2.value ? 1 : -1);
    this.dataGroups = dataGroups;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final lopts = [
      Menu.tlink("points", _("Points"), "models"),
      Menu.separator(),
      Menu.tlink("total", _("Total Prfs."), "models"),
      Menu.separator(),
      Menu.tlink("cash", _("Cash Prfs."), "models"),
      Menu.separator(),
      Menu.tlink("ref", _("Ref. Prfs."), "models")
    ];
    final menu = new Menu(lopts, [], typeSel);


    final groups = dataGroups.length;
    wg
      .removeAll()
      .add(menu.wg)
      .add(rowGroups(0, groups >= 4 ? 4 : groups))
    ;

    if (groups >= 4)
      wg
        .add(rowGroups(4, groups >= 8 ? 8 : groups))
      ;

    if (groups >= 8)
      wg
        .add(Q("hr"))
        .add(rowGroups(8, groups >= 13 ? 13 : groups))
      ;

    if (groups >= 13)
      wg
        .add(rowGroups(13, groups >= 18 ? 18 : groups))
      ;

    if (groups >= 18)
      wg
        .add(Q("hr"))
        .add(rowGroups(18, groups))
      ;
  }

  function rowGroups (start: Int, end: Int): Domo {
    return Q("table")
      .att("align", "center")
      .add(Q("tr")
        .adds(It.range(start, end).map(i -> Q("td")
            .add(groupTable(dataGroups[i]))
        ).to()))
    ;
  }

  function groupTable(values: ModelFloats): Domo {
    return Q("table")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .att("colspan", 2)
          .style("text-align:center")
          .text(Dt.toIso(Opt.get(Dt.from(values.date))))))
      .add(Q("tr")
        .add(Q("td")
          .klass("lhead")
          .text(_("Model")))
        .add(Q("td")
          .klass("rhead")
          .text(_("Points"))))
      .adds(values.values.map(e -> Q("tr")
        .add(Q("td")
          .klass("lframe")
          .text(e.model))
        .add(Q("td")
          .klass("rframe")
          .text(Dec.toIso(e.value, 2)))))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    final url = Ui.url();
    final type = url.exists("1") ? url.get("1").trim() : "";

    Cts.client.send([
      "source" => Js.ws("ModelsPg"),
      "rq" => Js.ws("idata"),
      "type" => Js.ws(type)
    ], rp -> {
      final type = rp["type"].rs();
      final dataGroups = rp["dataGroups"].ra().map(e -> ModelFloats.fromJs(e));

      new ModelsPg(wg, type, dataGroups).show();
    });
  }


}
