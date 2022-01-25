// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import cm.data.ModelEvals;
import I18n._;

/// Models page.
class ModelsPg {
  final wg: Domo;
  final evalsGroups: Array<ModelEvals>;

  function new (wg: Domo, evalsGroups: Array<ModelEvals>) {
    this.wg = wg;
    evalsGroups.sort((e1, e2) -> e1.date < e2.date ? 1 : -1);
    for (evs in evalsGroups)
      evs.evals.sort((e1, e2) -> e1.eval < e2.eval ? 1 : -1);
    this.evalsGroups = evalsGroups;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final groups = evalsGroups.length;
    wg
      .removeAll()
      .add(Q("table")
        .klass("flat")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .text("Models"))))
    ;
  }


  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("ModelsPg"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final evalsGroup = rp["evalsGroup"].ra().map(e -> ModelEvals.fromJs(e));

      new ModelsPg(wg, evalsGroup).show();
    });
  }


}
