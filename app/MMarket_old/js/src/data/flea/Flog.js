// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import LogEntry from "../LogEntry.js"; // eslint-disable-line

/**
    Fleas log.
**/
export default class Flog {
  /**
      @param {string} id
      @param {!Array<!LogEntry>} entries
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
      @return {!Array<!LogEntry>}
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
    const /** !Array<!LogEntry> */ es = js[1].map(e => LogEntry.fromJs(e));
    return new Flog(id, es);
  }
}
