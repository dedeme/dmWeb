// Copyright 30-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Eflea from "./Eflea.js";

/**
    Evaluated flea data + date.
**/
export default class EfleaDate {
  /* .
  _rc_ EfleaDate: serial
    date : string
    eflea: !Eflea
  */

  /*--*/
  /**
      @param {string} date
      @param {!Eflea} eflea
  **/
  constructor (date, eflea) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {!Eflea}
    **/
    this._eflea = eflea;

  }

  /**
      @return {string}
  **/
  get date () {
    return this._date;
  }

  /**
      @return {!Eflea}
  **/
  get eflea () {
    return this._eflea;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._date,
      this._eflea.toJs()
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!EfleaDate}
  **/
  static fromJs (serial) {
    return new EfleaDate(
      serial[0],
      Eflea.fromJs(serial[1])
    );
  }
  /*--*/
}
