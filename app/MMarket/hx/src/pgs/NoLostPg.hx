// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.Dt;
import dm.Dec;
import dm.It;
import dm.LineChart;
import data.Model;
import data.Broker;
import data.eval.ModelEval;
import data.eval.Result;
import data.OrderNL;
import I18n._;

/// Operations page.
class NoLostPg {
  final wg: Domo;
  final model: Model;
  final result: Result;
  final eval: ModelEval;
  final cos: Array<String>;
  final lastCloses: Array<Float>;
  final dates: Array<String>;
  final hassets: Array<Float>;
  final orders: Array<OrderNL>;

  function new (
    wg: Domo, model: Model, result: Result, eval: ModelEval,
    cos: Array<String>, lastCloses: Array<Float>,
    dates: Array<String>, hassets: Array<Float>, orders: Array<OrderNL>
  ) {
    this.wg = wg;
    this.model = model;
    this.result = result;
    this.eval = eval;
    this.cos = cos;
    this.lastCloses = lastCloses;
    this.dates = dates;
    this.hassets = hassets;
    this.orders = orders;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    var assets = 0.0;
    final trs: Array<Domo> = [];

    if (orders.length > 0) {
      var buys: Array<String> = [];
      var catchs: Array<String> = [];
      var sells: Array<String> = [];
      var cash = Cts.initialCapital;
      var lastDate = "";
      var coughts: Array<String> = [];
      final portfolio: Map<String, Int> = [];
      for (nk in cos) portfolio[nk] = 0;

      var tr = Q("tr");
      for (o in orders) {
        if (lastDate == "") {
          lastDate = o.date;
        } else if (o.date != lastDate && lastDate != "") {
          trs.push(mkTr(lastDate, buys, catchs, sells, portfolio, coughts, cash));
          buys = [];
          sells = [];
          catchs = [];
          lastDate = o.date;
        }
        if (o.type == OrderNL.SELL) {
          sells.push(o.nick);
          portfolio[o.nick] = portfolio[o.nick] - o.stocks;
          cash += Broker.sell(o.stocks, o.price);
          coughts = coughts.filter(nk -> nk != o.nick);
        } else if (o.type == OrderNL.BUY) {
          buys.push(o.nick);
          portfolio[o.nick] = portfolio[o.nick] + o.stocks;
          cash -= Broker.buy(o.stocks, o.price);
        } else {
          catchs.push(o.nick);
          coughts.push(o.nick);
        }
      }

      trs.push(mkTr(lastDate, buys, catchs, sells, portfolio, coughts, cash));
      assets = It.fromMap(portfolio)
        .filter(tp -> tp.e2 > 0)
        .reduce(cash, (r, tp) ->
            r + Broker.sell(tp.e2, lastCloses[cos.indexOf(tp.e1)])
          )
      ;
    } else {
      trs.push(Q("tr")
        .add(Q("td")
          .att("rowspan", "5")
          .text("Without Data")))
      ;
    }

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
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("chead")
            .text(_("Client")))
          .add(Q("td")
            .klass("chead")
            .text(_("Server"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(assets, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(Dec.toIso(hassets[hassets.length - 1], 2)))))
      .add(Q("div")
        .klass("head")
        .text(_("Orders")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(mkGr()))))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Date")))
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Buys")))
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Coughts")))
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Sells")))
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Portfolio")))
          .add(Q("td")
            .klass("lhead")
            .style("text-align:center")
            .text(_("Jail")))
          .add(Q("td")
            .klass("rhead")
            .style("text-align:center")
            .text(_("Cash"))))
        .adds(trs))
    ;
  }

  function mkTr (
    date: String,
    buys: Array<String>, catchs: Array<String>, sells: Array<String>,
    portfolio: Map<String, Int>, coughts: Array<String>, cash: Float
  ) {
    return Q("tr")
      .add(Q("td")
        .klass("lframe")
        .text(Dt.toIso(Opt.oget(Dt.from(date), Date.now()))))
      .add(Q("td")
        .klass("lframe")
        .style("width:100px")
        .text(buys.join(", ")))
      .add(Q("td")
        .klass("lframe")
        .style("width:100px")
        .text(catchs.join(", ")))
      .add(Q("td")
        .klass("lframe")
        .style("width:100px")
        .text(sells.join(", ")))
      .add(Q("td")
        .klass("lframe")
        .style("width:300px")
        .text(It.fromMap(portfolio)
            .filter(tp -> tp.e2 > 0)
            .map(tp -> tp.e1)
            .to()
            .join(", ")
          ))
      .add(Q("td")
        .klass("lframe")
        .style("width:200px")
        .text(coughts.join(", ")))
      .add(Q("td")
        .klass("rframe")
        .text(Dec.toIso(cash, 2)))
    ;
  }

  function mkGr (): Domo {
    final labels = dates.map(e -> e.substring(4, 6));
    final ch = LineChart.mk();
    ch.exArea.width = 600;
    ch.exArea.height = 300;
    ch.inPadding.left = 100;
    ch.inAtts.background = "#e9e9e9";
    ch.data = new LineChartData(
      labels,
      [hassets.map(e->Some(e))],
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
      "source" => Js.ws("NoLostPg"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId),
      "params" => switch (params) { case Some(p): p; case None: Js.wn(); }
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      final result = Result.fromJs(rp["result"]);
      final eval = ModelEval.fromJs(rp["eval"]);
      final nicks = rp["nicks"].ra().map(e-> e.rs());
      final lastCloses = rp["lastCloses"].ra().map(e -> e.rf());
      final dates = rp["dates"].ra().map(e-> e.rs());
      final assets = rp["assets"].ra().map(e -> e.rf());
      final orders = rp["orders"].ra().map(e -> OrderNL.fromJs(e));

      new NoLostPg(
        wg, model, result, eval, nicks, lastCloses, dates, assets, orders
      ).show();
    });
  }
}