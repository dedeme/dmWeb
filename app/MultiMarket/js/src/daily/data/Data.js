// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Co from "./Co.js";

/** Server data. */
export default class Data {
  /**
   * param {Object<?>} d
   */
  constructor (d) {
    this._state = d["state"];
    this._server = d["server"];
    this._cos = Co.mkMap(d["cos"]);
  }

  /**
   * Server state: Can be "active", "sleeping", "toActive", "toSleeping"
   * @return {string}
   */
  get state () {
    return this._state;
  }

  /**
   * Server name
   * @return {string}
   */
  get server () {
    return this._server;
  }

  /** @return {!Map<String, Co>} */
  get cos () {
    return this._cos;
  }
}

