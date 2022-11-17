// Copyright 16-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.LineChart;
import data.AccAnn;
import I18n._;

/// Stays page.
class Stays {
  final wg: Domo;
  final anns: Array<AccAnn>;

  function new (wg: Domo, anns: Array<AccAnn>) {
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
            .add(table()))))
    ;
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
      "source" => Js.ws("Stays"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(AccAnn.fromJs);
      trace(anns);
      new Stays(wg, anns).show();
    });
  }
}
