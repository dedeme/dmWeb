// Copyright 10-Dic-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Stat");

Stat = class {
  /**
   * @param {number} cash
   * @param {number} buys
   * @param {number} sells
   */
  constructor (cash, buys, sells) {
    /** @private */
    this._cash = cash;
    /** @private */
    this._buys = buys;
    /** @private */
    this._sells = sells;
  }

  /** @return {number} */
  cash () {
    return this._cash;
  }

  /** @return {number} */
  buys () {
    return this._buys;
  }

  /** @return {number} */
  sells () {
    return this._sells;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._cash,
      this._buys,
      this._sells
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Stat}
   */
  static restore (serial) {
    return new Stat (
      serial[0],
      serial[1],
      serial[2]
    );
  }
}


