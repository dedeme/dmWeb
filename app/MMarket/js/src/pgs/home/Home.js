// Copyright 23-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import LogRow from "../../data/LogRow.js";
import Log from "../../wgs/Log.js";
import Cts from "../../data/Cts.js";

const $ = e => Ui.$(e);

/**
    Main Home page.
**/
export default class Home {
  /**
      @private
      @param {!Domo} wg
      @param {!Domo} log
  **/
  constructor (wg, log) {
    this._wg = wg;
    this._log = log;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg.removeAll()
      .add(this._log)
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return {!Promise<!Home>}
  **/
  static async mk (wg) {
    /**
        @return {!Promise<!Array<!LogRow>>}
    **/
    async function load () {
      const rp = await Cts.client.send({
        "module": "home",
        "source": "home",
        "rq": "log",
      });
      const /** !Array<!Array<?>> */ log = rp["log"];
      return log.map(e => LogRow.fromJs(e));
    }

    /**
        @return {!Promise<void>}
    **/
    async function reset () {
      await Cts.client.send({
        "module": "home",
        "source": "home",
        "rq": "reset",
      });
    }

    const logDiv = $("div");
    await Log.mk(logDiv, load, reset);
    return new Home(wg, logDiv);
  }

}


