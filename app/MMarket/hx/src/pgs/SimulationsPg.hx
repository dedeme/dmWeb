// Copyright 29-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import data.Model;
import data.simulation.SimProfitsRow;
import I18n._;

/// Simulations page.
class SimulationsPg {
  final HTOTAL = 0;
  final HCASH = 1;
  final HREF = 2;
  final TOTAL = 3;
  final CASH = 4;
  final REF = 5;

  final wg: Domo;
  final model: Model;
  final pfs: Array<SimProfitsRow>;
  var order = 0;

  function new (wg: Domo, model: Model, pfs: Array<SimProfitsRow>) {
    this.wg = wg;
    this.model = model;
    pfs.sort((e1, e2) -> e1.hprofits.total < e2.hprofits.total ? 1 : -1);
    this.pfs = pfs;
    order = HTOTAL;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    pfs.sort(
        order == HTOTAL ?
          (e1, e2) -> e1.hprofits.total < e2.hprofits.total ? 1 : -1
      : order == HCASH ?
          (e1, e2) -> e1.hprofits.cash < e2.hprofits.cash ? 1 : -1
      : order == HREF ?
          (e1, e2) -> e1.hprofits.ref < e2.hprofits.ref ? 1 : -1
      : order == TOTAL ?
          (e1, e2) -> e1.profits.total < e2.profits.total ? 1 : -1
      : order == CASH ?
          (e1, e2) -> e1.profits.cash < e2.profits.cash ? 1 : -1
      :
          (e1, e2) -> e1.profits.ref < e2.profits.ref ? 1 : -1
    );

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td"))
          .adds(model.paramNames.map(n -> Q("td")
            .klass("rhead")
            .text(n)))
          .add(Q("td").klass("rhead"))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(HTOTAL))
              .klass("link")
              .text(_("H. Total"))))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(HCASH))
              .klass("link")
              .text(_("H. Cash"))))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(HREF))
              .klass("link")
              .text(_("H. Ref."))))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(TOTAL))
              .klass("link")
              .text(_("Total"))))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(CASH))
              .klass("link")
              .text(_("Cash"))))
          .add(Q("td")
            .klass("rhead")
            .add(Ui.link(() -> sortBy(REF))
              .klass("link")
              .text(_("Ref.")))))
        .adds(pfs.map(p -> Q("tr")
          .add(Q("td")
            .add(Q("a")
              .klass("link")
              .att(
                  "href",
                  "?" + model.id +
                  "&charts&" +
                  Js.wa(p.params.map(n-> Js.wf(n))).to()
                )
              .add(Ui.img("see"))))
          .adds(It.range(p.params.length).map(i -> Q("td")
              .klass("rframe")
              .text(Fns.paramFormatter(model.paramBaseIncs[i])(p.params[i]))
            ).to())
          .add(Q("td").klass("rhead"))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.hprofits.total, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.hprofits.cash, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.hprofits.ref, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.profits.total, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.profits.cash, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(p.profits.ref, 2)))))

        )
    ;
  }

  // Controls ------------------------------------------------------------------

  function sortBy (order: Int) {
    this.order = order;
    show();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, modelId: String): Void {
    Cts.client.send([
      "source" => Js.ws("SimulationsPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final profits = rp["profits"].ra().map(e -> SimProfitsRow.fromJs(e));
      new SimulationsPg(wg, model, profits).show();
    });
  }
}
