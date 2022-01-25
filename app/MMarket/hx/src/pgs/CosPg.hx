// Copyright 15-Jan-2022 ºDeme
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
import dm.ModalBox;
import data.Model;
import data.eval.ModelEval;
import data.eval.Result;
import I18n._;

/// Companies charts page.
class CosPg {
  final wg: Domo;
  final model: Model;
  final result: Result;
  final eval: ModelEval;
  final cos: Array<String>;

  var grDivs: Array<Array<Domo>> = [];
  var nlosses = 0;
  var ratiosSum = 0.0;

  final modalBox: ModalBox;
  final modalBoxDiv = Q("div");

  function new (
    wg: Domo, model: Model, result: Result, eval: ModelEval, cos: Array<String>
  ) {
    this.wg = wg;
    this.model = model;
    this.result = result;
    this.eval = eval;
    this.cos = cos;

    modalBox = new ModalBox(modalBoxDiv);
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    nlosses = 0;
    ratiosSum = 0.0;
    grDivs = [];

    final clientResultsDiv = Q("div");

    final nrows = Std.int((cos.length - 1) / 3) + 1;
    for (i in 0...nrows) {
      final row = [];
      for (j in 0...3) {
        final dv = Q("div");
        final ico = i * 3 + j;
        if (ico < cos.length) {
          final co = cos[ico];
          wait(dv, co);
        }
        row.push(dv);
      }
      grDivs.push(row);
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Companies")))
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
      .add(Q("div").klass("separator"))
      .add(clientResultsDiv)
      .add(Q("div").klass("separator2"))
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .adds(grDivs.map(r -> Q("tr")
            .adds(r.map(dv -> Q("td").add(dv)))
          )))
      .add(modalBox.wg)
    ;

    It.range(cos.length).eachSyn(
      mkGr,
      () -> {
        clientResultsDiv
          .removeAll()
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("rframe")
                .add(Q("span")
                  .text("" + (cos.length - nlosses) + " "))
                .add(Ui.img("profits")
                  .style("vertical-align: middle")))
              .add(Q("td")
                .klass("rframe")
                .add(Q("span")
                  .text("" + nlosses))
                .add(Ui.img("losses")
                  .style("vertical-align: middle")))
              .add(Q("td")
                .klass("rframe")
                .text(Dec.toIso(ratiosSum * 100 / cos.length, 2) + "%"))))
         ;
      }
    );

  }

  function wait (div: Domo, co: String): Void {
    div
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .text(co)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.img("wait.gif")))))
    ;
  }

  function mkGr (ico: Int, frec: () -> Void): Void {
    final co = cos[ico];
    final div = grDivs[Std.int(ico / 3)][ico % 3];

    Cts.client.send([
      "source" => Js.ws("CosPg"),
      "rq" => Js.ws("co"),
      "modelId" => Js.ws(model.id),
      "params" => Js.wa(eval.params.map(e -> Js.wf(e))),
      "co" => Js.ws(co)
    ], rp -> {
      final result = Result.fromJs(rp["result"]);
      final refsJs = rp["refs"].ra();
      var skip = refsJs.length;
      final refs = It.from(refsJs)
        .map(e -> e.rf())
        .dropWhile(e -> e <= 0)
        .to();
      skip -= refs.length;
      final dates = It.from(rp["dates"].ra()).drop(skip).map(e -> e.rs()).to();
      final labels = dates.map(e -> e.substring(4, 6));
      final qs = It.from(rp["qs"].ra()).drop(skip).map(e -> e.rf()).to();
      final refs1 = It.range(qs.length).map(i -> {
        final q = qs[i];
        final r = refs[i];
        return r > q ? Some(r) : None;
      }).to();
      final refs2 = It.range(qs.length).map(i -> {
        final q = qs[i];
        final r = refs[i];
        return r < q ? Some(r) : None;
      }).to();

      if (result.profits < 0) ++nlosses;
      ratiosSum += result.profits;
      final ch = LineChart.mk();
      ch.exArea.width = 300;
      ch.exArea.height = 150;
      ch.inAtts.background = "#e9e9e9";
      ch.data = new LineChartData(
        labels,
        [ refs1,
          refs2,
          qs.map(e -> Some(e))],
        [ new LineChartLine(1, "#4060a0", false),
          new LineChartLine(1, "#a06040", false),
          new LineChartLine(1, "#000000", false)
        ]
      );

      var prevLabel = labels[0];
      ch.data.drawLabel = (l, i) -> {
        if (i == 0) return false;
        if (l != prevLabel && (l == "01" || l == "05" || l == "09")) {
          prevLabel = l;
          return true;
        }
        return false;
      };
      ch.data.drawGrid = (l, i) -> false;

      div
        .removeAll()
        .add(Q("table")
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:left")
              .text(co))
            .add(Q("td")
              .style("text-align:right")
              .add(Q("span")
                .text(
                    Dec.toIso(result.profits * 100, 2) + "% [" +
                    Dec.toIso(result.sales, 0) + "] "
                  ))
              .add(Ui.img(result.profits < 0 ? "losses" : "profits")
                .style("vertical-align:middle"))))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .add(ch.mkWg()
                .on(CLICK, () -> showBigGr(co, ch))))))
      ;

      frec();
    });
  }

  function showBigGr (co: String, ch: LineChart): Void {
    ch.exArea.width = 800;
    ch.exArea.height = 400;
    modalBoxDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(co))
      .add(ch.mkWg())
      .add(Q("button")
        .text(_("Close"))
        .on(CLICK, () -> modalBox.show(false)))
    ;
    modalBox.show(true);
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
      "source" => Js.ws("CosPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
      "params" => switch (params) { case Some(p): p; case None: Js.wn(); }
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final result = Result.fromJs(rp["result"]);
      final eval = ModelEval.fromJs(rp["eval"]);
      final cos = rp["cos"].ra().map(e -> e.rs());
      new CosPg(wg, model, result, eval, cos).show();
    });
  }
}
