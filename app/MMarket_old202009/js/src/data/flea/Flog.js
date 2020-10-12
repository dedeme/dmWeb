// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import LogRow from "../LogRow.js"; // eslint-disable-line

/**
    Fleas log.
**/
export default class Flog {
  /**
      @param {string} id
      @param {!Array<!LogRow>} entries
  **/
  constructor (id, entries) {
    this._id = id;
    this._entries = entries;
  }

  /**
      @return {string}
  **/
  get id () {
    return this._id;
  }

  /**
      @return {!Array<!LogRow>}
  **/
  get entries () {
    return this._entries;
  }

  /**
      @param {!Array<?>} js
      @return {!Flog}
  **/
  static fromJs (js) {
    const /** string */ id = js[0];
    const /** !Array<!LogRow> */ es = js[1].map(e => LogRow.fromJs(e));
    return new Flog(id, es);
  }
}
