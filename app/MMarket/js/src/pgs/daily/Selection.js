// Copyright 08-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Storage management of daily charts selection.
**/

import Store from "../../dmjs/Store.js";

/**
    @private
**/
const key = "MMarket_daily_selection";

/**
    Storage management of daily charts selection.
**/
export default class Selection {
  /**
      @return !Array<string>
  **/
  static get nicks () {
    const v = Store.take(key);
    if (v === null) {
      return [];
    }
    return JSON.parse(v);
  }

  /**
      Returns 'true' if Selection contains 'nick'.
      @param {string} nick
      @return boolean
  **/
  static contains (nick) {
    return Selection.nicks.includes(nick);
  }

  /**
      Adds a company.
      @param {string} nick
      @return void
  **/
  static add (nick) {
    const nks = Selection.nicks;
    if (!nks.includes(nick)) {
      nks.push(nick);
      Store.put(key, JSON.stringify(nks));
    }
  }

  /**
      Removes a company.
      @param {string} nick
      @return void
  **/
  static remove (nick) {
    const nks = Selection.nicks;
    Store.put(key, JSON.stringify(nks.filter(e => e !== nick)));
  }

}
