// Copyright 10-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Order received in Orders page test.
**/
export default class Forder {
  /* .
  _rc_ Forder: from
    date  : string
    nick  : string
    isSell: boolean
    stocks: number
    price : number
  */

  /*--*/
  /**
      @param {string} date
      @param {string} nick
      @param {boolean} isSell
      @param {number} stocks
      @param {number} price
  **/
  constructor (
    date,
    nick,
    isSell,
    stocks,
    price
  ) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {string}
    **/
    this._nick = nick;

    /**
        @private
        @type {boolean}
    **/
    this._isSell = isSell;

    /**
        @private
        @type {number}
    **/
    this._stocks = stocks;

    /**
        @private
        @type {number}
    **/
    this._price = price;

  }

  /**
      @return {string}
  **/
  get date () {
    return this._date;
  }

  /**
      @return {string}
  **/
  get nick () {
    return this._nick;
  }

  /**
      @return {boolean}
  **/
  get isSell () {
    return this._isSell;
  }

  /**
      @return {number}
  **/
  get stocks () {
    return this._stocks;
  }

  /**
      @return {number}
  **/
  get price () {
    return this._price;
  }

  /**
      @param {!Array<?>} serial
      @return {!Forder}
  **/
  static fromJs (serial) {
    return new Forder(
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4]
    );
  }
  /*--*/
}

