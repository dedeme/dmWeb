// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Flea data.
**/
export default class Flea {
  /* .
  _rc_ Flea: serial
    date    : string
    cycle   : number
    id      : number
    params  : !Array<number>
  */

  /*--*/
  /**
      @param {string} date
      @param {number} cycle
      @param {number} id
      @param {!Array<number>} params
  **/
  constructor (date, cycle, id, params) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {number}
    **/
    this._cycle = cycle;

    /**
        @private
        @type {number}
    **/
    this._id = id;

    /**
        @private
        @type {!Array<number>}
    **/
    this._params = params;

  }

  /**
      @return {string}
  **/
  get date () {
    return this._date;
  }

  /**
      @return {number}
  **/
  get cycle () {
    return this._cycle;
  }

  /**
      @return {number}
  **/
  get id () {
    return this._id;
  }

  /**
      @return {!Array<number>}
  **/
  get params () {
    return this._params;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._date,
      this._cycle,
      this._id,
      this._params
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Flea}
  **/
  static fromJs (serial) {
    return new Flea(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/

  /**
      @return {string}
  **/
  get name () {
    return this._date + "-" + String(this._cycle) + "-" + String(this._id);
  }

}
