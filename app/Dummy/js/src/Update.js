// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

/** Update page. */
export default class Update {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show("settings", $("div").style("text-align:center")
      .add($("h2").html(_("Update")))
      .add($("p").att("align", "center").html(_("Update")))
    );
  }
}

