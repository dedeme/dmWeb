// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import data.Model;
import data.eval.ModelEval;
import I18n._;

/// Results page.
class ResultsPg {
  final wg: Domo;
  final model: Model;
  final evals: Array<ModelEval>;

  function new (wg: Domo, model: Model, evals: Array<ModelEval>) {
    this.wg = wg;
    this.model = model;
    evals.sort((e1, e2) -> e1.hvalue < e2.hvalue ? 1 : -1);
    this.evals = evals;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
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
            .text(_("H. Eval.")))
          .add(Q("td")
            .klass("rhead")
            .text(_("H. Sales")))
          .add(Q("td")
            .klass("rhead")
            .text(_("Eval.")))
          .add(Q("td")
            .klass("rhead")
            .text(_("Sales"))))
        .adds(evals.map(eval -> Q("tr")
          .add(Q("td")
            .add(Q("a")
              .klass("link")
              .att(
                  "href",
                  "?" + model.id +
                  "&charts&" +
                  Js.wa(eval.params.map(n-> Js.wf(n))).to()
                )
              .add(Ui.img("see"))))
          .adds(It.range(eval.params.length).map(i -> Q("td")
              .klass("rframe")
              .text(Fns.paramFormatter(model.paramBaseIncs[i])(eval.params[i]))
            ).to())
          .add(Q("td").klass("rhead"))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(eval.hvalue * 1000, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(eval.hsales, 0)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(eval.value * 1000, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(eval.sales, 0)))))

        )
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, modelId: String): Void {
    Cts.client.send([
      "source" => Js.ws("ResultsPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final evals = rp["evals"].ra().map(e -> ModelEval.fromJs(e));
      new ResultsPg(wg, model, evals).show();
    });
  }
}
