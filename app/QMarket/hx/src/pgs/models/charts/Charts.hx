// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.charts;

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
import data.model.Model;
import data.model.Order;
import pgs.models.wgs.HistoricChart;
import I18n._;

/// Model chart.
class Charts {
  var wg: Domo;
  var model: Model;
  var menuSel: String;
  /// Constructor.
  ///   wg      : Container.
  ///   modelId : Flea model identifier.
  ///   isAsset : Returns true if chart is of assets.
  ///   eflea   : Evaluated flea.
  public function new (
    wg: Domo, isAsset: Bool, model: Model
  ) {
    this.wg = wg;
    menuSel = isAsset ? "assets" : "companies";
    this.model = model;

    view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  function view () {
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
      Q("td").klass("header").text(_("Id")),
      Q("td").klass("header").text(_("Jump"))
    ];
    final paramsBody = [
      Q("td").klass("menu").text("" + model.id),
      Q("td").klass("fparam").text(Fns.nformat(model.param, Cts.paramDecs))
    ];


    final rsHead = [
      Q("td").klass("header").text(_("Assets")),
      Q("td").klass("header").text(_("Pf. Avg")),
      Q("td").klass("header").text(_("Eval.")),
      Q("td").klass("header").text(_("Buys")),
      Q("td").klass("header").text(_("Sales")),
      Q("td").klass("header"),
      Q("td").klass("header").text(_("H. Eval.")),
      Q("td").klass("header").text(_("H. Sales"))
    ];
    final ev = model.evaluation;
    final hev = model.hevaluation;
    final rsBody = [
      Q("td").klass("fnumber").text(Fns.nformat(ev.assets, 2)),
      Q("td").klass("fnumber").text(Fns.nformat(ev.profitsAvg, 4)),
      Q("td").klass("fnumber").text(Fns.nformat(ev.value * 1000, 2)),
      Q("td").klass("fnumber").text(Fns.nformat(ev.buys, 0)),
      Q("td").klass("fnumber").text(Fns.nformat(ev.sales, 0)),
      Q("td").klass("menu"),
      Q("td").klass("fnumber").text(Fns.nformat(hev.value * 1000, 2)),
      Q("td").klass("fnumber").text(Fns.nformat(ev.sales, 0))
    ];

    final wg = Q("div");
    if (menuSel == "assets") {
      this.assets(wg);
    } else if (menuSel == "orders") {
      this.orders(wg);
    } else {
      Companies.mk(wg, model);
    }

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(_("Qlevel") + "-" + model.qlevel + " [" + model.id + "]"))
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
      "module" => Js.ws("models"),
      "source" => Js.ws("charts"),
      "rq" => Js.ws("assets"),
      "qlevel" => Js.wi(model.qlevel),
      "id" => Js.wi(model.id)
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
      "module" => Js.ws("models"),
      "source" => Js.ws("charts"),
      "rq" => Js.ws("ordersData"),
      "qlevel" => Js.wi(model.qlevel),
      "id" => Js.wi(model.id)
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
            .text(Fns.nformat(cash, 2)))
        ;
      }

      //-----------------------------

      final orders = rp["orders"].ra().map(e -> Order.fromJs(e));
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
              .text(Fns.nformat(assets, 2)))))
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
