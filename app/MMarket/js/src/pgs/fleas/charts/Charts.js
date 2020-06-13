// Copyright 01-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Model charts.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import {Menu} from "../../../dmjs/Menu.js";
import Cts from "../../../data/Cts.js";
import {_} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Eflea from "../../../data/flea/Eflea.js"; //eslint-disable-line
import Companies from "./Companies.js";
import HistoricChart from "../wgs/HistoricChart.js";

const $ = e => Ui.$(e);

/**
    Model charts.
**/
export default class Charts {

  /**
      @param {!Domo} wg
      @param {string} modelId
      @param {boolean} isAsset
      @param {!Array<string>} parNames
      @param {!Array<number>} parFmts
      @param {!Eflea} eflea
  **/
  constructor (wg, modelId, isAsset, parNames, parFmts, eflea) {
    this._wg = wg;
    this._modelId = modelId;
    this._menuSel = isAsset ? "assets" : "companies";
    this._parNames = parNames;
    this._parFmts = parFmts;
    this._eflea = eflea;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const eflea = this._eflea;
    const flea = eflea.flea;
    const lopts = [
      Menu.toption("assets", _("Assets"), () => { this.menu("assets") }),
      Menu.separator(),
      Menu.toption(
        "companies", _("Companies"), () => { this.menu("companies") }
      )
    ];
    const ropts = [];
    const menu = new Menu(lopts, ropts, this._menuSel);

    const paramsHead = [
      $("td").klass("header").text(_("Model")),
      $("td").klass("header").text(_("Id"))
    ];
    this._parNames.forEach(e => {
      paramsHead.push($("td").klass("header").text(e));
    });
    const paramsBody = [
      $("td").klass("menu").text(this._modelId),
      $("td").klass("menu").text(flea.name)
    ];
    for (let i = 0; i < this._parFmts.length; ++i) {
      paramsBody.push($("td").klass("fparam").text(Cts.nformat(
        flea.params[i], this._parFmts[i]
      )));
    }

    const rsHead = [
      $("td").klass("header").text(_("Assets")),
      $("td").klass("header").text(_("Pf. Avg")),
      $("td").klass("header").text(_("Pf. Var")),
      $("td").klass("header").text(_("Eval.")),
      $("td").klass("header").text(_("Buys")),
      $("td").klass("header").text(_("Sells")),
    ];
    const rsBody = [
      $("td").klass("fnumber").text(Cts.nformat(eflea.assets, 2)),
      $("td").klass("fnumber").text(Cts.nformat(eflea.profitsAvg, 4)),
      $("td").klass("fnumber").text(Cts.nformat(eflea.profitsVa, 4)),
      $("td").klass("fnumber").text(Cts.nformat(eflea.ev * 1000, 2)),
      $("td").klass("fnumber").text(Cts.nformat(eflea.buys, 0)),
      $("td").klass("fnumber").text(Cts.nformat(eflea.sells, 0))
    ];

    const wg = $("div");
    if (this._menuSel === "assets") {
      this.assets(wg);
    } else {
      Companies.mk(wg, this._modelId, flea.params);
    }

    this._wg
      .removeAll()
      .add(menu.wg)
      .add($("div")
        .klass("head")
        .text(flea.name))
      .add($("table")
        .att("align", "center")
        .klass("white")
        .add($("tr")
          .adds(paramsHead))
        .add($("tr")
          .adds(paramsBody)))
      .add($("div")
        .style("height:5px"))
      .add($("table")
        .att("align", "center")
        .klass("white")
        .add($("tr")
          .adds(rsHead))
        .add($("tr")
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
  menu (option) {
    this._menuSel = option;
    this.view();
  }

  /**
      @private
      @param {!Domo} wg
      @return !Promise<void>
  **/
  async assets (wg) {
    let chart = new HistoricChart(true, []).wg;

    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "charts", // Reusing call
      "rq": "assets",
      "modelId": this._modelId,
      "params": this._eflea.flea.params
    });

    if (rp["ok"]) {
      const dates = rp["dates"];
      const assets = rp["assets"];
      const qs = [];
      for (let i = 0; i < dates.length; i++) {
        qs.push([dates[i], Math.round(assets[i])]);
      }
      chart = new HistoricChart(true, qs).wg;
    }

    wg
      .removeAll()
      .add($("div")
        .klass("head")
        .text(_("Assets")))
      .add($("table")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .add(chart
              .klass("frame")))))
    ;
  }

}

