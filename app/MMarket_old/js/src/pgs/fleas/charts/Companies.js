// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Model from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Investor from "../../../data/flea/Investor.js"; //eslint-disable-line
import InvestorWg from "../../../wgs/InvestorWg.js";

const $ = e => Ui.$(e);

/**
    Companies of flea charts.
**/
export default class Companies {
  /**
      @private
      @param {!Client} client
      @param {!Investor} investor
      @param {?} data
  **/
  constructor (client, investor, data) {
    this._client = client;
    this._investor = investor;
    this._data = data;

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
      .add(new InvestorWg(this._investor).wg);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Investor} investor
      @return {!Promise<!Companies>}
  **/
  static async mk (client, investor) {
    const rp = await client.send({
      "module": "Fleas",
      "source": "Companies",
      "rq": "idata",
      "modelId": investor.model.id,
      "eflea": investor.eflea.toJs()
    });
    return new Companies(client, investor, rp["data"]);
  }

}
