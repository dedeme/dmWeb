// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Dec from "../../../dmjs/Dec.js";
import {_, _args} from "../../../I18n.js";
import Model from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Params from "../../../wgs/Params.js";
import HistoricChart from "../../../wgs/HistoricChart.js";
import Operations from "../../../wgs/Operations.js";

const $ = e => Ui.$(e);

/**
    References fleas tests page.
**/
export default class References {
  /**
      @private
      @param {!Client} client
      @param {!Model} model
      @param {!Array<string>} nickList
  **/
  constructor (client, model, nickList) {
    nickList.sort((s1, s2) => s1 > s2 ? 1 : -1);
    this._client = client;
    this._model = model;
    this._nickList = nickList;
    this._nickSel = 0;
    this._wgParams = new Params(
      model.parNames, model.parMins, model.parMaxs, "par", "showBt"
    );
    this._chartDiv = $("div");
    this._ordersDiv = $("div");
    this._resultDiv = $("div");
    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const ns = this._nickSel;
    const nkRows = [];
    let row = $("tr");
    this._nickList.forEach((nk, i) => {
      if (i !== ns) nk = (nk + "   ").substring(0, 6).replace(/\s/g, "&nbsp;");
      else nk = (nk + " ").substring(0, 4).replace(/\s/g, "&nbsp;");
      if (i % 10 === 0 && i !== 0) {
        nkRows.push(row);
        row = $("tr");
      }
      row.add($("td").style("text-align:left")
        .add(Ui.link(() => { this.selNick(i) })
          .klass(i === ns ? "link frame" : "link")
          .setStyle("font-family", "monospace").html(nk))
      );
    });
    const mod = this._nickList.length % 10;
    if (mod > 0) {
      for (let i = 0; i < (10 - mod); ++i) row.add($("td"));
      nkRows.push(row);
    } else {
      nkRows.push(row);
    }

    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:50%;text-align:center")
            .add($("div").klass("head").text(_("Companies")))
            .add($("table").klass("border").att("align", "center")
              .style("padding:5px")
              .adds(nkRows)))
          .add($("td").style("width:50%;text-align:center")
            .add($("div").klass("head").text(_("Parameters")))
            .add($("table").att("align", "center").add($("tr").add($("td")
              .add(this._wgParams.wg))))))
        .add($("tr")
          .add($("td").att("colspan", 2).style("text-align:center")
            .add($("div").style("height:5px"))
            .add($("button").att("id", "showBt").text(_("Show"))
              .on("click", () => this.showChart()))
            .add($("div").style("height:5px"))
            .add(this._chartDiv.removeAll()
              .add(new HistoricChart(true, []).wg.klass("frame0")))
            .add($("div").style("height:5px"))
            .add(this._ordersDiv.removeAll())
            .add($("div").style("height:5px"))
            .add(this._resultDiv.removeAll())))
      );
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async showChart () {
    const rp = await this._client.send({
      "module": "Fleas",
      "source": "References",
      "rq": "chartData",
      "modelId": this._model.id,
      "nickName": this._nickList[this._nickSel],
      "params": this._wgParams.value
    });
    const dates = rp["dates"];
    const closes = rp["closes"];
    const opens = rp["opens"];
    const refs = rp["refs"];
    const qs = [];
    for (let i = 0; i < dates.length; i++) {
      qs.push([dates[i], closes[i], refs[i]]);
    }
    this._chartDiv.removeAll()
      .add(new HistoricChart(true, qs).wg.klass("frame0"));

    this._ordersDiv.removeAll()
      .add(new Operations(dates, opens, closes, refs).wg.klass("frame0"));

    this._resultDiv.removeAll()
      .add($("div")
        .text(_args(
          _("Profits calculated by server: %0"),
          new Dec(rp["profits"], 4).toIso()
        )));
  }

  /**
      @private
      @param {number} i Index of nicks
  **/
  selNick (i) {
    this._nickSel = i;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Model} model
      @return {!Promise<!References>}
  **/
  static async mk (client, model) {
    const rp = await client.send({
      "module": "Fleas",
      "source": "References",
      "rq": "nickList"
    });
    return new References(client, model, rp["nickList"]);
  }

}
