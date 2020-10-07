// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.charts;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.Js;
import dm.Menu;
import data.Cts;
import data.flea.Eflea;
import pgs.fleas.wgs.HistoricChart;
import I18n._;

/// Model chart.
class Charts {
  var wg: Domo;
  var modelId: String;
  var parNames: Array<String>;
  var parFmts: Array<Int>;
  var eflea: Eflea;
  var menuSel: String;
  /// Constructor.
  ///   wg      : Container.
  ///   modelId : Flea model identifier.
  ///   isAsset : Returns true if chart is of assets.
  ///   parNames: Parameter names.
  ///   parFmts : Parameter format. Each parameter is:
  ///             0 - Integer
  ///             4, 6 - Percentage.
  ///             Other - Normal number with 'other' decimals positions.
  ///   eflea   : Evaluated flea.
  public function new (
    wg: Domo, modelId: String, isAsset: Bool,
    parNames: Array<String>, parFmts: Array<Int>, eflea: Eflea
  ) {
    this.wg = wg;
    this.modelId = modelId;
    menuSel = isAsset ? "assets" : "companies";
    this.parNames = parNames;
    this.parFmts = parFmts;
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
      Menu.toption("companies", _("Companies"), () -> menu("companies"))
    ];
    final ropts = [];
    final menu = new Menu(lopts, ropts, menuSel);

    final paramsHead = [
      Q("td").klass("header").text(_("Model")),
      Q("td").klass("header").text(_("Id"))
    ];
    for (e in parNames) paramsHead.push(Q("td").klass("header").text(e));
    final paramsBody = [
      Q("td").klass("menu").text(modelId),
      Q("td").klass("menu").text(flea.name)
    ];
    for (i in 0...parFmts.length) {
      paramsBody.push(Q("td").klass("fparam").text(Cts.nformat(
        flea.params[i], parFmts[i]
      )));
    }

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
      Q("td").klass("fnumber").text(Cts.nformat(eflea.ev * 1000, 2)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.buys, 0)),
      Q("td").klass("fnumber").text(Cts.nformat(eflea.sells, 0))
    ];

    final wg = Q("div");
    if (menuSel == "assets") {
      this.assets(wg);
    } else {
      Companies.mk(wg, modelId, flea.params);
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

  /**
      @private
      @param {string} option
      @return void
  **/
  function menu (option: String) {
    menuSel = option;
    this.view();
  }

  /**
      @private
      @param {!Domo} wg
      @return !Promise<void>
  **/
  function assets (wg: Domo) {
    var chart = new HistoricChart(true, []).wg;

    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("charts"),
      "rq" => Js.ws("assets"),
      "modelId" => Js.ws(modelId),
      "params" => Js.wa(eflea.flea.params.map(e -> Js.wf(e)))
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

}
