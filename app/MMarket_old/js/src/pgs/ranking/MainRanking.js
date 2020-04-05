// Copyright 10-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Path from "../../data/Path.js"; //eslint-disable-line
import HidingMenu from "../../pgs/main/HidingMenu.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Main Home page.
**/
export default class MainRanking {
  /**
      @private
      @param {!Client} client
      @param {!HidingMenu} menu
  **/
  constructor (client, menu) {
    this._client = client;
    this._upMenu = menu;

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
      .add($("div").html("Ranking"))
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Path} path
      @param {!HidingMenu} menu
      @return {!MainRanking}
  **/
  static mk (client, path, menu) {
    return new MainRanking(client, menu);
  }

}


