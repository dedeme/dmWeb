// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Quote");

Quote = class {
  /**
   * @param {string} date
   * @param {number} open
   * @param {number} close
   * @param {number} max
   * @param {number} min
   * @param {number} vol
   * @param {boolean} force If is true, Quote will be set as valid although
   *  its values do not match control standars.
   */
  constructor (date, open, close, max, min, vol, force) {
    /** @private */
    this._date = date;
    /** @private */
    this._open = open;
    /** @private */
    this._close = close;
    /** @private */
    this._max = max;
    /** @private */
    this._min = min;
    /** @private */
    this._vol = vol;
    /** @private */
    this._force = force;
  }

  /** @return {string} */
  date () {
    return this._date;
  }

  /** @return {number} */
  open () {
    return this._open;
  }

  /** @return {number} */
  close () {
    return this._close;
  }

  /** @return {number} */
  max () {
    return this._max;
  }

  /** @return {number} */
  min () {
    return this._min;
  }

  /** @return {number} */
  vol () {
    return this._vol;
  }

  /** @return {boolean} */
  force () {
    return this._force;
  }

  /** @return {string} */
  toString () {
    return this._date + ":" + this._open + ":" + this._close + ":" +
      this._max + ":" + this._min + ":" + this._vol + ":" + this._force;
  }

  /**
   * @param {string} s Result of Quote.toString()
   * @return {!Quote}
   */
  static from (s) {
    const q = s.split(":");
    return new Quote (
      q[0].trim(),
      +q[1].trim(),
      +q[2].trim(),
      +q[3].trim(),
      +q[4].trim(),
      +q[5].trim(),
      q[6].trim() === "true"
    );
  }
}
