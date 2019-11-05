// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Menu from "./dmjs/Menu.js";
import Ui from "./dmjs/Ui.js";
import Main from "./Main.js";
import {_} from "./I18n.js";
import League from "./League.js";

const $ = e => Ui.$(e);

/**
    Leagues page.
**/
export default class Leagues {

  /**
      @param {!Main} main
  **/
  constructor (main) {
    this._main = main;
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
    this._main.view.removeAll().add(this._menuDiv).add(this._view);
    this.update();
  }

  // CONTROL -------------------------------------------------------------------

  /**
      @return {!Promise}
  **/
  async update () {
    const rq = {
      "page": "Leagues",
      "rq": "sessions"
    };
    const /** !Object<string, Array<string>> */ rp = await Main.client.rq(rq);
    const /** Array<string> */ sessions = rp["sessions"];

    const menu = this._menu;
    menu.addLeft($("span").text(_("Sessions") + ": "));
    sessions.sort().reverse().forEach((s, i) => {
      if (i > 0) menu.addLeft(Menu.separator());
      menu.addLeft(Menu.mkOption(s, s, () => this.go(s)));
    });
    menu.setSelected(sessions[0]);
    this._menuDiv.removeAll().add(menu.wg);
    this.go(sessions[0]);
  }

  /**
      @private
      @param {string} session
  **/
  go (session) {
    new League(this, session).show();
  }

}
