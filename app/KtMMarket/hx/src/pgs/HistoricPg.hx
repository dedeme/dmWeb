// Copyright 04-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.It;
import dm.Dec;
import dm.LineChart;
import data.Model;
import data.eval.ModelEval;
import data.eval.Result;
import I18n._;


/// Historic chart page.
class HistoricPg {
  final wg: Domo;
  final model: Model;
  final result: Result;
  final dates: Array<String>;
  final eval: ModelEval;
  final assets: Array<Float>;
  final withdrawals: Array<Float>;

  function new (
    wg: Domo, model: Model, result: Result,
    dates: Array<String>, eval: ModelEval,
    assets: Array<Float>, withdrawals: Array<Float>
  ) {
    this.wg = wg;
    this.model = model;
    this.result = result;
    this.dates = dates;
    this.eval = eval;
    this.assets = assets;
    this.withdrawals = withdrawals;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Historic")))
      .add(Q("div").klass("separator"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("chead")
            .text(_("Model")))
          .adds(model.paramNames.map(n -> Q("td")
              .klass("rhead")
              .text(n)
            )))
        .add(Q("tr")
          .add(Q("td")
            .klass("cframe")
            .text(model.name))
          .adds(It.range(eval.params.length).map(i -> Q("td")
              .klass("rframe")
              .text(Fns.paramFormatter(model.paramBaseIncs[i])(eval.params[i]))
            ).to())))
      .add(Q("div").klass("separator2"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Assets")))
          .add(Q("td")
            .klass("rhead")
            .text(_("Profits (%)")))
          .add(Q("td")
            .klass("rhead")
            .text(_("Eval.")))
          .add(Q("td")
            .klass("rhead")
            .text(_("Sales"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(result.assets, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(result.profits * 100, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(result.eval() * 1000, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(result.sales, 0)))))
      .add(Q("div").klass("separator2"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
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
        .add(Q("tr")
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
      .add(Q("div")
        .klass("head")
        .text(_("Assets")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(mkGr(true)))))
      .add(Q("div")
        .klass("head")
        .text(_("Withdrawals")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(mkGr(false)))))
    ;
  }

  function mkGr (isAssets: Bool): Domo {
    final labels = dates.map(e -> e.substring(4, 6));
    final ch = LineChart.mk();
    ch.exArea.width = 600;
    ch.exArea.height = isAssets ? 300 : 150;
    ch.inPadding.left = 100;
    ch.inAtts.background = "#e9e9e9";
    ch.data = new LineChartData(
      labels,
      isAssets ? [assets.map(e->Some(e))] : [withdrawals.map(e->Some(e))],
      [ new LineChartLine(1, "#000000", false)]
    );
    ch.data.round = 0;

    var prevLabel = labels[0];
    ch.data.drawLabel = (l, i) -> {
      if (i == 0) return false;
      if (l != prevLabel && (l == "01" || l == "04" || l == "07"|| l == "10")) {
        prevLabel = l;
        return true;
      }
      return false;
    };
    ch.data.drawGrid = ch.data.drawLabel;

    return ch.mkWg();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, modelId: String): Void {
    final url = Ui.url();
    var params = None;
    if (url.exists("2")) {
      try {
        params = Some(Js.from(url["2"]));
        Opt.eget(params).ra().map(e -> e.rf());
      } catch (e) {
        params = None;
      }
    }
    Cts.client.send([
      "source" => Js.ws("HistoricPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
      "params" => switch (params) { case Some(p): p; case None: Js.wn(); }
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final result = Result.fromJs(rp["result"]);
      final dates = rp["dates"].ra().map(e-> e.rs());
      final eval = ModelEval.fromJs(rp["eval"]);
      final assets = rp["assets"].ra().map(e -> e.rf());
      final withdrawals = rp["withdrawals"].ra().map(e -> e.rf());
      new HistoricPg(wg, model, result, dates, eval, assets, withdrawals).show();
    });
  }
}
