// Copyright 07-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Daily chart data
**/

export class AccData {
  /* .
  _rc_ AccData: from
    stocks: number
    price : number
    ref   : number
  */

  /*--*/
  /**
      @param {number} stocks
      @param {number} price
      @param {number} ref
  **/
  constructor (stocks, price, ref) {

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

    /**
        @private
        @type {number}
    **/
    this._ref = ref;

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
      @return {number}
  **/
  get ref () {
    return this._ref;
  }

  /**
      @param {!Array<?>} serial
      @return {!AccData}
  **/
  static fromJs (serial) {
    return new AccData(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

}

export class DailyChart {
  /* .
  _rc_ DailyChart: from
    nick   : string
    close  : number
    hours  : !Array<number>
    quotes : !Array<number>
    accData: !Array<!AccData>
  */

  /*--*/
  /**
      @param {string} nick
      @param {number} close
      @param {!Array<number>} hours
      @param {!Array<number>} quotes
      @param {!Array<!AccData>} accData
  **/
  constructor (
    nick,
    close,
    hours,
    quotes,
    accData
  ) {

    /**
        @private
        @type {string}
    **/
    this._nick = nick;

    /**
        @private
        @type {number}
    **/
    this._close = close;

    /**
        @private
        @type {!Array<number>}
    **/
    this._hours = hours;

    /**
        @private
        @type {!Array<number>}
    **/
    this._quotes = quotes;

    /**
        @private
        @type {!Array<!AccData>}
    **/
    this._accData = accData;

  }

  /**
      @return {string}
  **/
  get nick () {
    return this._nick;
  }

  /**
      @return {number}
  **/
  get close () {
    return this._close;
  }

  /**
      @return {!Array<number>}
  **/
  get hours () {
    return this._hours;
  }

  /**
      @return {!Array<number>}
  **/
  get quotes () {
    return this._quotes;
  }

  /**
      @return {!Array<!AccData>}
  **/
  get accData () {
    return this._accData;
  }

  /**
      @param {!Array<?>} serial
      @return {!DailyChart}
  **/
  static fromJs (serial) {
    return new DailyChart(
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4].map(e =>
        AccData.fromJs(e)
      )
    );
  }
  /*--*/

}


