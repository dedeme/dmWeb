// Copyright 30-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Menu from "./dmjs/Menu.js";
import Ui from "./dmjs/Ui.js";
import Main from "./Main.js";
import Leagues from "./Leagues.js"; // eslint-disable-line
import {_} from "./I18n.js";

const $ = e => Ui.$(e);

/**
    League page.
**/
export default class League {
  /**
      @param {!Leagues} leagues
      @param {string} session
  **/
  constructor (leagues, session) {
    this._leagues = leagues;
    this._session = session;

    /**
        @type {!Menu}
    **/
    this._menu = new Menu(false);
    this._menuDiv = $("div");
    this._view = $("div");
  }

  /**
      @return {!Domo}
  **/
  get view () {
    return this._view;
  }

  // VIEW ----------------------------------------------------------------------

  /**
      @return {void}
  **/
  show () {
    this._leagues.view.removeAll().add(this._menuDiv).add(this._view);
    this.update();
  }

  // CONTROL -------------------------------------------------------------------

  /**
      @return {!Promise}
  **/
  async update () {
    const rq = {
      "page": "League",
      "rq": "rounds",
      "session": this._session
    };
    const /** !Object<string, Array<string>> */ rp = await Main.client.rq(rq);
    const /** Array<string> */ rounds = rp["rounds"];

    const menu = this._menu;
    menu.addLeft($("table").add($("tr").add($("td").text(_("Rounds") + ": "))));
    rounds.sort().reverse().forEach((r, i) => {
      if (i > 0) menu.addLeft(Menu.separator());
      menu.addLeft(Menu.mkOption(r, r, () => this.go(r)));
    });
    menu.setSelected(rounds[0]);
    this._menuDiv.removeAll().add(menu.wg);
//    this.go(rounds[0]);
  }

  /**
      @private
      @param {string} round
  **/
  go (round) {
    alert("to " + round);
  }

}
