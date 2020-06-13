// Copyright 01-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Model companies charts.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Dec from "../../../dmjs/Dec.js";
import ModalBox from "../../../dmjs/ModalBox.js";
import Cts from "../../../data/Cts.js";
import {_} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Eflea from "../../../data/flea/Eflea.js"; //eslint-disable-line
import Frank from "../../../data/flea/Frank.js"; //eslint-disable-line
import HistoricChart from "../wgs/HistoricChart.js";

const $ = e => Ui.$(e);

async function mkChart (modelId, params, nickName) {

  function mk (profits, chart) {
    const wg = $("div");
    return $("table")
      .add($("tr")
        .add($("td")
          .style("text-align:left")
          .text(nickName))
        .add($("td")
          .style("text-align:right")
          .add($("span")
            .html(new Dec(profits * 100, 2).toIso() + "%&nbsp;&nbsp;"))
          .add(Ui.img("win").style("vertical-align:middle"))))
      .add($("tr")
        .add($("td")
          .att("colspan", 2)
          .add(wg)
          .add(chart
            .klass("frame")
            .setStyle("cursor", "pointer")
            .on("click", async () => {
              const rp = await Cts.client.send({
                "module": "fleas",
                "source": "ftests/references", // Reuse call
                "rq": "chartData",
                "modelId": modelId,
                "nickName": nickName,
                "params": params
              });
              let chart = new HistoricChart(true, []).wg;
              if (rp["ok"]) {
                const dates = rp["dates"];
                const closes = rp["closes"];
                const refs = rp["refs"];
                const qs = [];
                for (let i = 0; i < dates.length; i++) {
                  qs.push([dates[i], closes[i], refs[i]]);
                }

                chart = new HistoricChart(true, qs).wg;
              }
              const mb = new ModalBox(
                $("table")
                  .add($("tr")
                    .add($("td")
                      .style("text-align:center")
                      .text(nickName)))
                  .add($("tr")
                    .add($("td")
                      .add(chart
                        .klass("frame"))))
                  .add($("tr")
                    .add($("td")
                      .style("text-align:center")
                      .add($("button")
                        .text(_("Close"))
                        .on("click", () => { mb.show(false) }))))
              );
              wg
                .removeAll()
                .add(mb.wg)
              ;
              mb.show(true);
            }))))
    ;
  }

  const rp = await Cts.client.send({
    "module": "fleas",
    "source": "ftests/references", // Reuse call
    "rq": "chartData",
    "modelId": modelId,
    "nickName": nickName,
    "params": params
  });

  if (!rp["ok"]) {
    return [mk(0, new HistoricChart(false, []).wg), 0];
  }

  const dates = rp["dates"];
  const closes = rp["closes"];
  const refs = rp["refs"];
  const profits = rp["profits"];
  const qs = [];
  for (let i = 0; i < dates.length; i++) {
    qs.push([dates[i], closes[i], refs[i]]);
  }

  return [mk(profits, new HistoricChart(false, qs).wg), profits];
}

/**
    Model companies charts.
**/
export default class Companies {
  /**
      @param {!Domo} wg
      @param {string} modelId
      @param {!Array<number>} params
      @param {!Array<string>} nicks
  **/
  constructor (wg, modelId, params, nicks) {
    this._wg = wg;
    this._modelId = modelId;
    this._params = params;
    this._nicks = nicks;
    this._nicks.sort();

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  async view () {
    const modelId = this._modelId;
    const params = this._params;

    const summary = $("table").att("align", "center");
    const chs = $("table").att("align", "center").klass("frame");
    this._wg
      .removeAll()
      .add($("div")
        .klass("head")
        .text(_("Profits")))
      .add(summary)
      .add(chs)
    ;

    function addSeparator () {
      return chs.add($("tr")
        .add($("td")
          .att("colspan", 3)
          .html("<hr>")))
      ;
    }

    const n = this._nicks.length;
    let sumProfs = 0;
    let sumWins = 0;
    let sumLooses = 0;
    let tr = $("tr");
    for (let i = 0; i < n; ++i) {
      const nickName = this._nicks[i];
      const rs = await mkChart(modelId, params, nickName);
      const chart = rs[0];
      const profits = rs[1];
      sumProfs += profits;
      if (profits < 0) ++sumLooses;
      else ++sumWins;

      switch (i % 3) {
      case 0:
        addSeparator();
        tr = $("tr");
        tr.add($("td").add(chart));
        break;
      case 2:
        tr.add($("td").add(chart));
        chs.add(tr);
        break;
      default:
        tr.add($("td").add(chart));
      }
    }

    switch (n % 3) {
    case 1: chs.add(tr.add($("td")).add($("td"))); break;
    case 2: chs.add(tr.add($("td"))); break;
    }
    addSeparator();

    const profits = sumProfs / n;
    summary.add($("tr")
      .add($("td")
        .add(Ui.img("win").style("vertical-align:middle"))
        .add($("span").html(`: ${sumWins} | `))
        .add(Ui.img("loose").style("vertical-align:middle"))
        .add($("span").html(
          `: ${sumLooses} | % : ${new Dec(profits * 100, 2).toIso()}%`
        ))))
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {string} modelId
      @param {!Array<number>} params
      @return Promise<!Companies>
  **/
  static async mk (wg, modelId, params) {
    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "ftests/references", // Reusing call
      "rq": "nickList"
    });
    const nicks = rp["nickList"];

    return new Companies(wg, modelId, params, nicks);
  }

}
