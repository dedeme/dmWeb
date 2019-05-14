// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Log from "./wgs/Log.js";

const $ = Ui.$;

/** Sys Home page. */
export default class Home {

  /**
   * @param {!SysMain} sysMain
   */
  constructor (sysMain) {
    this._sysMain = sysMain;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._logDiv = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  show () {
    this._sysMain.view.removeAll().add(
      $("div").style("text-align:center")
        .add($("div").klass("head").text(_("Home")))
        .add($("table").klass("frame").att("align", "center")
          .add($("tr").add($("td").style("text-align:left")
            .add($("a").att("href", Main.urlBase + "?market")
              .text(_("Market")))
            .add($("br"))
            .add($("a").att("href", Main.urlBase + "?daily")
              .text(_("Daily Quotes")))
            .add($("br"))
            .add($("a").att("href", Main.urlBase + "?fleas")
              .text(_("Fleas Data"))))))
        .add(this._logDiv)
    );

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "sys",
      "source": "Home",
      "rq": "getLog"
    };
    const rp = await Main.client.send(rq);
    const log = rp["log"];

    this._logDiv.removeAll().add(new Log(log).wg); //eslint-disable-line
  }
}
