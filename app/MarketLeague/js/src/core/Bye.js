// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import {_args, _} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;
/** Bye page. */
export default class Bye {
  /**
   * @param {!Main} main Main page
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
    this._main.view.removeAll().add(
      $("div")
        .add($("div").klass("title")
          .html(`&nbsp;<br>${Main.app}<br>&nbsp;`))
        .add($("div")
          .add($("table")
            .att("class", "border")
            .att("width", "100%")
            .att("style",
              "background-color: #f8f8f8;" +
              "border-collapse: collapse;")
            .add($("tr")
              .add($("td")
                .att("style", "padding:0px 10px 0px 10px;")
                .html(_args(_("Logout-message"), Main.app))))))
    );
  }
}
