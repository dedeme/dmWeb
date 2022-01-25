// Copyright 15-Jan-2022 ºDeme
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
import I18n._args;

/// Hot map page.
class HotPg {
  final wg: Domo;
  final model: Model;
  final evals: Array<ModelEval>;
  final max: Float;
  final min: Float;

  function new (wg: Domo, model: Model, evals: Array<ModelEval>) {
    this.wg = wg;
    this.model = model;
    this.evals = evals;

    max = evals[0].hvalue;
    min = max;
    for (eval in evals) {
      if (eval.hvalue > max) max = eval.hvalue;
      if (eval.hvalue < min) min = eval.hvalue;
    }
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final npars = model.paramNames.length;
    wg
      .removeAll()
      .add(npars == 1 ? oneParam() : npars == 2 ? twoParam() : moreParams(npars))
    ;
  }

  function oneParam (): Domo {
    return Q("table")
      .klass("flat")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(model.paramNames[0]))
        .add(Q("td")))
      .adds(evals.map(eval->
        Q("tr")
          .add(Q("td")
            .klass("rframe")
            .text(Fns.paramFormatter(model.paramBaseIncs[0])(eval.params[0])))
          .add(Q("td")
            .klass("border")
            .style(
              "width:50px;" +
              "cursor:pointer;" +
              "background:" + color(max, min, eval.hvalue)
            ))
            .on(CLICK, () -> js.Browser.location.assign(
              "?" + model.id + "&charts&" +
              Js.wa([Js.wf(eval.params[0])]).to()
            ))
      ))
    ;
  }

  function twoParam (): Domo {
    final pm1 = evals[0].params[0];
    final ncols = It.from(evals).takeWhile(eval->
      Dec.eq(eval.params[0], pm1, 0.000001)
    ).count();
    final row = [0.0];
    for (j in 0...ncols) row.push(evals[j].params[1]);
    final rows = [row];
    for (i in 0...Std.int(evals.length / ncols)) {
      final row = [evals[i * ncols].params[0]];
      for (j in 0...ncols) {
        row.push(evals[i * ncols + j].hvalue);
      }
      rows.push(row);
    }

    return Q("table")
      .klass("flat")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .klass("rhead")
          .text(model.paramNames[0]))
        .adds(It.range(ncols).map(i -> Q("td")).to()))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(model.paramNames[1]))
        .add(Q("td"))
        .adds(It.range(1, ncols).map(i -> Q("td")
            .klass("rframe")
            .text(Fns.paramFormatter(model.paramBaseIncs[1])(rows[0][i]))
          )))
      .adds(It.range(1, rows.length).map(i -> Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .klass("rframe")
            .text(Fns.paramFormatter(model.paramBaseIncs[0])(rows[i][0])))
          .adds(It.range(1, ncols).map(j -> Q("td")
              .klass("border")
              .att("title", Dec.toIso(rows[i][j] * 1000, 2))
              .style(
                  "cursor:pointer;" +
                  "background:" + color(max, min, rows[i][j])
                )
              .on(CLICK, () -> js.Browser.location.assign(
                  "?" + model.id + "&charts&" +
                  Js.wa([
                    Js.wf(rows[i][0]),
                    Js.wf(rows[0][j])
                  ]).to()
                ))
            ))
        ))
    ;
  }

  function moreParams (n: Int): Domo {
    return Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .text(_args(_("There is no hot map for %0 parameters"),["" + n]))))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, modelId: String): Void {
    Cts.client.send([
      "source" => Js.ws("HotPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final evals = rp["evals"].ra().map(e -> ModelEval.fromJs(e));
      new HotPg(wg, model, evals).show();
    });
  }

  static function color (max: Float, min: Float, value: Float): String {
    final df = max - min;
    final red = Std.int((max - value) * 256 / df );
    final blue = Std.int((value - min) * 256 / df );
    return "rgb(" + red + ",80," + blue + ")";
  }
}
