// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Investor from "../../../data/flea/Investor.js"; //eslint-disable-line
import SummaryData from "../../../data/flea/SummaryData.js";
import InvestorWg from "../../../wgs/InvestorWg.js";
import {_} from "../../../I18n.js";
import SummaryChart from "../../../wgs/SummaryChart.js";

const $ = e => Ui.$(e);

/**
    Sumary of fleas charts.
**/
export default class Summary {
  /**
      @private
      @param {!Client} client
      @param {!Investor} investor
      @param {boolean} isRanking
      @param {string} error
      @param {!Array<string>} dates
      @param {!Array<number>} assetss
      @param {!Array<number>} profitss
      @param {!Array<number>} markets
  **/
  constructor (
    client, investor, isRanking, error, dates, assetss, profitss, markets
  ) {
    this._client = client;
    this._investor = investor;
    this._isRanking = isRanking;
    this._error = error;
    this._dates = dates;
    this._assetss = assetss;
    this._profitss = profitss;
    this._markets = markets;

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
    this._wg.removeAll()
      .add(new InvestorWg(this._investor).wg)
      .add($("div").klass("separator"));

    if (this._error !== "") {
      this._wg.add($("table").att("align", "center").add($("tr")
        .add($("td").klass("frame").text(this._error))));
      return;
    }

    const assetsChar = new SummaryChart(this._dates, this._assetss);
    const profitsChar = new SummaryChart(this._dates, this._profitss);
    const marketChar = new SummaryChart(this._dates, this._markets);

    this._wg
      .add($("div").klass("head").text(_("Assets")))
      .add($("table").att("align", "center").add($("tr")
        .add($("td").add(assetsChar.wg).klass("frame0")
          .style("background:#d9d9d9"))))
      .add($("div").klass("separator"))
      .add($("div").klass("head").text(_("Profits")))
      .add($("table").att("align", "center").add($("tr")
        .add($("td").add(profitsChar.wg).klass("frame0")
          .style("background:#d9d9d9"))))
      .add($("div").klass("head").text(_("Market")))
      .add($("table").att("align", "center").add($("tr")
        .add($("td").add(marketChar.wg).klass("frame0")
          .style("background:#d9d9d9"))))
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Investor} investor
      @param {boolean} isRanking
      @return {!Promise<!Summary>}
  **/
  static async mk (client, investor, isRanking) {
    const rp = await client.send({
      "module": "Fleas",
      "source": "Summary",
      "rq": "idata",
      "modelId": investor.model.id,
      "flea": investor.eflea.flea.toJs()
    });
    const error = rp["error"];
    const dates = [];
    const assetss = [];
    const profitss = [];
    const markets = [];
    if (error === "") {
      const hist = rp["historic"].map(e => SummaryData.fromJs(e));
      hist.forEach(e => {
        dates.push(e.date);
        assetss.push(e.assets);
        profitss.push(e.profits);
        markets.push(e.market);
      });
    }

    return new Summary(
      client, investor, isRanking, error, dates, assetss, profitss, markets
    );
  }

}
