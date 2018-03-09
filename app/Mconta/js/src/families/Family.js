// Copyright 4-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("families_Family");
goog.require("Gen");

families_Family = class {
  /**
   * @param {number} opt
   * @param {Gen} gen
   */
  constructor (opt, gen) {
    /** @private */
    this._opt = opt;
    /** @private */
    this._gen = gen;
  }

  /** @return {number} */
  opt () {
    return this._opt;
  }

  /** @return {Gen} */
  gen () {
    return this._gen;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._opt,
      this._gen.serialize()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!families_Family}
   */
  static restore (serial) {
    return new families_Family (
      serial[0],
      Gen.restore(serial[1])
    );
  }
}
