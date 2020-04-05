// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Main fleas charts page.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Maybe from "../../../dmjs/Maybe.js";
import {Menu} from "../../../dmjs/Menu.js";
import {_, _args} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Eflea from "../../../data/flea/Eflea.js";
import Investor from "../../../data/flea/Investor.js";
import Summary from "./Summary.js";
import Companies from "./Companies.js";

const $ = e => Ui.$(e);

/**
    Main Fleas charts page.
**/
export default class MainCharts {
  /**
      @private
      @param {!Client} client
      @param {!Investor} investor
      @param {boolean} isRanking
  **/
  constructor (client, investor, isRanking) {
    this._client = client;
    this._investor = investor;
    this._isRanking = isRanking;
    this._selSubmenu = "summary";

    this._wgChild = $("div");
    this._wg = $("div");
    this.showSummary();
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
    const lopts = [
      Menu.toption("summary", _("Summary"), () => this.showSummary()),
      Menu.separator(),
      Menu.toption("companies", _("Companies"), () => this.showCompanies()),
    ];
    const subMenu = new Menu(lopts, [], this._selSubmenu);

    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add(subMenu.wg)
            .add(this._wgChild))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async showSummary () {
    this._selSubmenu = "summary";
    const pg = await Summary.mk(this._client, this._investor, this._isRanking);
    this._wgChild.removeAll().add(pg.wg);
    this.view();
  }

  /**
      @private
  **/
  async showCompanies () {
    this._selSubmenu = "companies";
    const pg = await Companies.mk(this._client, this._investor);
    this._wgChild.removeAll().add(pg.wg);
    this.view();
  }

  // Static --------------------------------------------------------------------
  /**
      @param {!Client} client
      @param {!Fmodel} model
      @param {Maybe<!Eflea>} eflea
      @param {boolean} isRanking
      @return {!Promise<!Maybe<!MainCharts>>}
  **/
  static async mk (client, model, eflea, isRanking) {

    if (eflea.isJust()) {
      return Maybe.just(new MainCharts(
        client, new Investor(model, eflea.fromJust()), isRanking
      ));
    }

    const rp = await client.send({
      "module": "Fleas",
      "source": "MainCharts",
      "rq": "bestRanking",
      "modelId": model.id
    });
    eflea = Maybe.fromJs(rp["eflea"], Eflea.fromJs);
    if (eflea.isNothing()) {
      alert(_args(_("Best flea not found in '%0 (%1)'"), model.name, model.id));
      return Maybe.nothing;
    }
    return Maybe.just(new MainCharts(
      client, new Investor(model, eflea.fromJust()), true
    ));
  }

}


