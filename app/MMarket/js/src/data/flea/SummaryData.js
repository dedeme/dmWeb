// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Summary charts entry data.
**/
export default class SummaryData {
  /* .
  _rc_ SummaryData: from
    date   : string
    assets : number
    profits: number
    market : number
  */

  /*--*/
  /**
      @param {string} date
      @param {number} assets
      @param {number} profits
      @param {number} market
  **/
  constructor (date, assets, profits, market) {

    /**
        @private
        @type {string}
    **/
    this._date = date;

    /**
        @private
        @type {number}
    **/
    this._assets = assets;

    /**
        @private
        @type {number}
    **/
    this._profits = profits;

    /**
        @private
        @type {number}
    **/
    this._market = market;

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
  get assets () {
    return this._assets;
  }

  /**
      @return {number}
  **/
  get profits () {
    return this._profits;
  }

  /**
      @return {number}
  **/
  get market () {
    return this._market;
  }

  /**
      @param {!Array<?>} serial
      @return {!SummaryData}
  **/
  static fromJs (serial) {
    return new SummaryData(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/

}
