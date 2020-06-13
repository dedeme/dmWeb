// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Results of model_assets
**/

export default class AssetsRs {
  /* .
  _rc_ AssetsRs: from
    # Amount
    assets: number
    # Buys number
    buys: number
    # Sells number
    sells: number
  */

  /*--*/
  /**
      @param {number} assets Amount
      @param {number} buys Buys number
      @param {number} sells Sells number
  **/
  constructor (assets, buys, sells) {

    /**
        @private
        @type {number}
    **/
    this._assets = assets;

    /**
        @private
        @type {number}
    **/
    this._buys = buys;

    /**
        @private
        @type {number}
    **/
    this._sells = sells;

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
  get buys () {
    return this._buys;
  }

  /**
      @return {number}
  **/
  get sells () {
    return this._sells;
  }

  /**
      @param {!Array<?>} serial
      @return {!AssetsRs}
  **/
  static fromJs (serial) {
    return new AssetsRs(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

}
