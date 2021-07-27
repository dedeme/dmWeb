// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.charts;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Js;
import dm.Menu;
import data.Cts;
import data.BrokerF;
import data.flea.Eflea;
import data.flea.Forder;
import pgs.fleas.wgs.HistoricChart;
import I18n._;

/// Model chart.
class Charts {
  var wg: Domo;
  var modelId: String;
  var parName: String;
  var eflea: Eflea;
  var menuSel: String;
  /// Constructor.
  ///   wg      : Container.
  ///   modelId : Flea model identifier.
  ///   isAsset : Returns true if chart is of assets.
  ///   parName : Parameter name.
  ///   eflea   : Evaluated flea.
  public function new (
    wg: Domo, modelId: String, isAsset: Bool,
    parName: String, eflea: Eflea
  ) {
    this.wg = wg;
    this.modelId = modelId;
    menuSel = isAsset ? "assets" : "companies";
    this.parName = parName;
    this.eflea = eflea;

    view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  function view () {
    final flea = eflea.flea;
    final lopts = [
      Menu.toption("assets", _("Assets"), () -> menu("assets")),
      Menu.separator(),
      Menu.toption("orders", _("Orders"), () -> menu("orders")),
      Menu.separator(),
      Menu.toption("companies", _("Companies"), () -> menu("companies"))
    ];
    final ropts = [];
    final menu = new Menu(lopts, ropts, menuSel);

    final paramsHead = [
      Q("td").klass("header").text(_("Model")),
      Q("td").klass("header").text(_("Id")),
      Q("td").klass("header").text(parName)
    ];
    final paramsBody = [
      Q("td").klass("menu").text(modelId),
      Q("td").klass("menu").text(flea.name)
    ];
    paramsBody.push(Q("td").klass("fparam").text(Cts.nformat(
      flea.param, Cts.paramDecs
    )));


    final rsHead = [
      Q("td").klass("header").text(_("Assets")),
      Q("td").klass("header").text(_("Pf. Avg")),
      Q("td").klass("header").text(_("Pf. Var")),
      Q("td").klass("header").text(_("Eval.")),
      Q("td").klass("header").text(_("Buys")),
      Q("td").klass("header").text(_("Sells")),
    ];
    final rsBody = [
      Q("td").klass("fnumber").text(Cts.nformat(eflea.assets, 2)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.profitsAvg, 4)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.profitsVa, 4)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.eval * 1000, 2)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.buys, 0)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.sales, 0))
    ];

    final wg = Q("div");
    if (menuSel == "assets") {
      this.assets(wg);
    } else if (menuSel == "orders") {
      this.orders(wg);
    } else {
      Companies.mk(wg, modelId, flea.param);
    }

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(flea.name))
      .add(Q("table")
        .att("align", "center")
        .klass("white")
        .add(Q("tr")
          .adds(paramsHead))
        .add(Q("tr")
          .adds(paramsBody)))
      .add(Q("div")
        .style("height:5px"))
      .add(Q("table")
        .att("align", "center")
        .klass("white")
        .add(Q("tr")
          .adds(rsHead))
        .add(Q("tr")
          .adds(rsBody)))
      .add(wg)
    ;
  }

  // Control -------------------------------------------------------------------

  function menu (option: String) {
    menuSel = option;
    this.view();
  }

  function assets (wg: Domo) {
    var chart = new HistoricChart(true, []).wg;

    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("charts"),
      "rq" => Js.ws("assets"),
      "modelId" => Js.ws(modelId),
      "param" => Js.wf(eflea.flea.param)
    ], rp -> {
      if (rp["ok"].rb()) {
        final dates = rp["dates"].ra().map(e -> e.rs());
        final assets = rp["assets"].ra().map(e -> e.rf());
        final qs = [];
        for (i in 0...dates.length) {
          qs.push(new HistoricChartEntry(dates[i], Math.round(assets[i])));
        }
        chart = new HistoricChart(true, qs).wg;
      }

      wg
        .removeAll()
        .add(Q("div")
          .klass("head")
          .text(_("Assets")))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .add(chart
                .klass("frame")))))
      ;
    });
  }

  function orders (wg: Domo) {
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("ftests/orders"), // Reused
      "rq" => Js.ws("ordersData"),        // Reused
      "modelId" => Js.ws(modelId),
      "param" => Js.wf(eflea.flea.param)
    ], rp -> {
      function mkTr (
        date: String, buys: Array<String>, sells: Array<String>,
        portfolio: Map<String, Int>, cash: Float
      ) {
        return Q("tr")
          .add(Q("td")
            .klass("menu")
            .text(Dt.toIso(Opt.oget(Dt.from(date), Date.now()))))
          .add(Q("td")
            .klass("menu")
            .style("width:100px")
            .text(buys.join(", ")))
          .add(Q("td")
            .klass("menu")
            .style("width:100px")
            .text(sells.join(", ")))
          .add(Q("td")
            .klass("menu")
            .style("width:400px")
            .text(It.fromMap(portfolio)
                .filter(tp -> tp.e2 > 0)
                .map(tp -> tp.e1)
                .to()
                .join(", ")
              ))
          .add(Q("td")
            .klass("fnumber")
            .text(Cts.nformat(cash, 2)))
        ;
      }

      //-----------------------------

      final orders = rp["orders"].ra().map(e -> Forder.fromJs(e));
      final nicks = rp["nicks"].ra().map(e -> e.rs());
      final lastCloses = rp["lastCloses"].ra().map(e -> e.rf());

      var assets = 0.0;
      final trs: Array<Domo> = [];

      if (orders.length > 0) {
        var buys: Array<String> = [];
        var sells: Array<String> = [];
        var cash = Cts.initialCapital;
        var lastDate = "";
        final portfolio: Map<String, Int> = [];
        for (nk in nicks) portfolio[nk] = 0;

        var tr = Q("tr");
        for (o in orders) {
          if (lastDate == "") {
            lastDate = o.date;
          } else if (o.date != lastDate && lastDate != "") {
            trs.push(mkTr(lastDate, buys, sells, portfolio, cash));
            buys = [];
            sells = [];
            lastDate = o.date;
          }
          if (o.isSell) {
            sells.push(o.nick);
            portfolio[o.nick] = portfolio[o.nick] - o.stocks;
            cash += BrokerF.sell(o.stocks, o.price);
          } else {
            buys.push(o.nick);
            portfolio[o.nick] = portfolio[o.nick] + o.stocks;
            cash -= BrokerF.buy(o.stocks, o.price);
          }
        }

        trs.push(mkTr(lastDate, buys, sells, portfolio, cash));
        assets = It.fromMap(portfolio)
          .filter(tp -> tp.e2 > 0)
          .reduce(cash, (r, tp) ->
              r + BrokerF.sell(tp.e2, lastCloses[nicks.indexOf(tp.e1)])
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
          .text(_("Assets")))
        .add(Q("table")
          .att("align", "center")
          .klass("white")
          .add(Q("tr")
            .add(Q("td")
              .klass("fnumber")
              .text(Cts.nformat(assets, 2)))))
        .add(Q("div")
          .klass("head")
          .text(_("Orders")))
        .add(Q("table")
          .att("align", "center")
          .klass("white")
          .add(Q("tr")
            .add(Q("td")
              .klass("header")
              .style("text-align:center")
              .text(_("Date")))
            .add(Q("td")
              .klass("header")
              .style("text-align:center")
              .text(_("Buys")))
            .add(Q("td")
              .klass("header")
              .style("text-align:center")
              .text(_("Sells")))
            .add(Q("td")
              .klass("header")
              .style("text-align:center")
              .text(_("Portfolio")))
            .add(Q("td")
              .klass("header")
              .style("text-align:center")
              .text(_("Cash"))))
          .adds(trs))
      ;
    });
  }

}
