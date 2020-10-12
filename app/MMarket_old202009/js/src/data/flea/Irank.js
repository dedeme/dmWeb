// Copyright 31-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Investor from "./Investor.js";

/**
    Ranking of Investors.
**/
export default class Irank {
  /* .
  _rc_ Irank: from
    date: string
    invs: !Array<!Investor>
  */

  /*--*/
  /**
      @param {string} date
      @param {!Array<!Investor>} invs
  **/
  constructor (date, invs) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {!Array<!Investor>}
    **/
    this._invs = invs;

  }

  /**
      @return {string}
  **/
  get date () {
    return this._date;
  }

  /**
      @return {!Array<!Investor>}
  **/
  get invs () {
    return this._invs;
  }

  /**
      @param {!Array<?>} serial
      @return {!Irank}
  **/
  static fromJs (serial) {
    return new Irank(
      serial[0],
      serial[1].map(e =>
        Investor.fromJs(e)
      )
    );
  }
  /*--*/
}
