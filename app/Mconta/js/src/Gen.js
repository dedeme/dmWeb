// Copyright 10-Dic-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Gen");

Gen = class {
  /**
   * @param {number} maxOptions
   * @param {number} actualOption
   */
  constructor (maxOptions, actualOption) {
    /** @private */
    this._maxOptions = maxOptions;
    /** @private */
    this._actualOption = actualOption;
  }

  /** @return {number} */
  maxOptions () {
    return this._maxOptions;
  }

  /** @return {number} */
  actualOption () {
    return this._actualOption;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._maxOptions,
      this._actualOption
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Gen}
   */
  static restore (serial) {
    return new Gen (
      serial[0],
      serial[1]
    );
  }
}


