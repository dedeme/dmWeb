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
import dm.Dec;
import dm.Menu;
import data.Broker;
import data.Model;
import data.AssetsRs;
import data.Order;
import wgs.Params;
import wgs.Msg;
import pgs.models.wgs.HistoricChart;
import I18n._;

/// Model chart.
class Charts {
  final wg: Domo;
  final model: Model;
  final params: Array<Float>;
  final dates: Array<String>;
  final assets: Array<Float>;
  final withdrawals: Array<Float>;
  final results: AssetsRs;
  final nicks: Array<String>;
  final orders: Array<Order>;
  final lastCloses: Array<Float>;
  var menuSel: String;
  /// Constructor.
  ///   wg         : Container.
  ///   model      : Model.
  ///   params     : Parameters.
  ///   dates      : Historic dates.
  ///   assets     : Assets historic.
  ///   withdrawals: Withdrawal historic.
  ///   results    : Results.
  ///   nicks      : Nicks list.
  ///   orders     : Orders list.
  ///   lastCloses: Final comapany closes.
  public function new (
    wg: Domo, model: Model, params: Array<Float>,
    dates: Array<String>, assets: Array<Float>, withdrawals: Array<Float>,
    results: AssetsRs, nicks: Array<String>, orders: Array<Order>,
    lastCloses: Array<Float>
  ) {
    this.wg = wg;
    this.model = model;
    this.params = params;
    this.dates = dates;
    this.assets = assets;
    this.withdrawals = withdrawals;
    this.results = results;
    this.nicks = nicks;
    this.orders = orders;
    this.lastCloses = lastCloses;
    menuSel = "assets";

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

    final paramsHead = [Q("td").klass("header").text(_("Model"))];
    for (i in 0...params.length)
      paramsHead.push(Q("td").klass("header").text(model.paramNames[i]));
    final paramsBody = [
      Q("td")
        .klass("menu")
        .add(Ui.link(changeParams)
          .klass("link")
          .text(model.id))
    ];
    for (i in 0...params.length) {
      paramsBody.push(
        Q("td").klass("fparam").text(Fns.nformat(params[i], model.paramDecs[i]))
      );
    }

    final rsHead = [
      Q("td").klass("header").text(_("Assets")),
      Q("td").klass("header").text(_("Buys")),
      Q("td").klass("header").text(_("Sales"))
    ];
    final rsBody = [
      Q("td").klass("fnumber").text(Fns.nformat(results.assets, 2)),
      Q("td").klass("fnumber").text(Fns.nformat(results.buys, 0)),
      Q("td").klass("fnumber").text(Fns.nformat(results.sells, 0))
    ];

    final wg = Q("div");
    if (menuSel == "assets") {
      this.assetsView(wg);
    } else if (menuSel == "orders") {
      this.ordersView(wg);
    } else {
      new Companies(wg, model, params, nicks);
    }

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(model.name))
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

  function changeParams () {
    final wgParams = new Params(
      model.paramNames, model.paramMins, model.paramMaxs, "par", "showBt",
      params
    );
    Msg.showWg(wgParams.wg, () -> {
      final newPars = wgParams.value;
      var dif = false;
      for (i in 0...params.length)
        if (!Dec.eq(params[i], newPars[i], 0.0000001)) dif = true;
      if (dif) {
        js.Browser.location.assign(
          "?models&" + model.id + "&charts&" + Js.wa(newPars.map(Js.wf)).to()
        );
      }
    });
  }

  function assetsView (wg: Domo) {
    final qs = [];
    for (i in 0...dates.length) {
      qs.push(new HistoricChartEntry(dates[i], Math.round(assets[i])));
    }
    final chart = new HistoricChart(true, qs).wg;
    final qs2 = [];
    for (i in 0...dates.length) {
      qs2.push(new HistoricChartEntry(dates[i], Math.round(withdrawals[i])));
    }
    final chart2 = new HistoricChart(true, qs2).wg;

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
      .add(Q("div")
        .klass("head")
        .text(_("Withdrawals")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(chart2
              .klass("frame")))))
    ;
  }

  function ordersView (wg: Domo) {
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
        if (o.type == Order.SELL) {
          sells.push(o.nick);
          portfolio[o.nick] = portfolio[o.nick] - o.stocks;
          cash += Broker.sell(o.stocks, o.price);
        } else if (o.type == Order.BUY) {
          buys.push(o.nick);
          portfolio[o.nick] = portfolio[o.nick] + o.stocks;
          cash -= Broker.buy(o.stocks, o.price);
        }
      }

      trs.push(mkTr(lastDate, buys, sells, portfolio, cash));
      assets = It.fromMap(portfolio)
        .filter(tp -> tp.e2 > 0)
        .reduce(cash, (r, tp) ->
            r + Broker.sell(tp.e2, lastCloses[nicks.indexOf(tp.e1)])
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
  }

}
