// Copyright 06-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import LogEntry from "../../data/LogEntry.js";
import Log from "../../wgs/Log.js";

const $ = e => Ui.$(e);

/**
    Main Home page.
**/
export default class MainHome {
  /**
      @private
      @param {!Domo} log
  **/
  constructor (log) {
    this._log = log;

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
      .add(this._log)
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @return {!Promise<!MainHome>}
  **/
  static async mk (client) {
    /**
        @return {!Promise<!Array<!LogEntry>>}
    **/
    async function load () {
      const rp = await client.send({
        "module": "Home",
        "rq": "log",
      });
      const /** !Array<!Array<?>> */ log = rp["log"];
      log.reverse();
      return log.map(e => LogEntry.fromJs(e));
    }

    /**
        @return {!Promise<void>}
    **/
    async function reset () {
      await client.send({
        "module": "Home",
        "rq": "reset",
      });
    }

    const log = await Log.mk(true, true, load, reset);
    return new MainHome(log.wg);
  }

}


