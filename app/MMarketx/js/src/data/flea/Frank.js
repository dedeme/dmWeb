// Copyright 31-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Eflea from "./Eflea.js";

/**
    Ranking of Evaluated flea.
**/
export default class Frank {
  /* .
  _rc_ Frank: serial
    date : string
    efleas: !Array<!Eflea>
  */

  /*--*/
  /**
      @param {string} date
      @param {!Array<!Eflea>} efleas
  **/
  constructor (date, efleas) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {!Array<!Eflea>}
    **/
    this._efleas = efleas;

  }

  /**
      @return {string}
  **/
  get date () {
    return this._date;
  }

  /**
      @return {!Array<!Eflea>}
  **/
  get efleas () {
    return this._efleas;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._date,
      this._efleas.map(e => e.toJs())
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Frank}
  **/
  static fromJs (serial) {
    return new Frank(
      serial[0],
      serial[1].map(e =>
        Eflea.fromJs(e)
      )
    );
  }
  /*--*/
}
