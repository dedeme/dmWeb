// Copyright 15-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.LineChart;
import data.MonthAnn;
import I18n._;

/// Bills page.
class Bills {
  final wg: Domo;
  final anns: Array<MonthAnn>;

  function new (wg: Domo, anns: Array<MonthAnn>) {
    this.wg = wg;
    this.anns = anns;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(chart())))
        .add(Q("tr")
          .add(Q("td")
            .add(table()))))
    ;
  }

  function chart (): Domo {
    return LineChart.mk().mkWg();
  }

  function table (): Domo {
    final tb = Q("table").att("align", "center").klass("border");

    return anns.length == 0
      ? tb
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center")
            .html(_("Without Data"))))
      : tb
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center")
            .html(_("With Data"))))
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("HBills"),
      "source" => Js.ws("Bills"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(MonthAnn.fromJs);
      new Bills(wg, anns).show();
    });
  }
}
