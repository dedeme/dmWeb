// Copyright 04-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Nick */
export default class Quote {

  /* .
  _rc_ Quote : from
  date : string
  open : number
  close : number
  max : number
  min : number
  vol : number
  error : boolean
  */

  /*--*/
  /**
   * @param {string} date
   * @param {number} open
   * @param {number} close
   * @param {number} max
   * @param {number} min
   * @param {number} vol
   * @param {boolean} error
   */
  constructor (
    date,
    open,
    close,
    max,
    min,
    vol,
    error
  ) {

    /**
     * @private
     * @type {string}
     */
    this._date = date;

    /**
     * @private
     * @type {number}
     */
    this._open = open;

    /**
     * @private
     * @type {number}
     */
    this._close = close;

    /**
     * @private
     * @type {number}
     */
    this._max = max;

    /**
     * @private
     * @type {number}
     */
    this._min = min;

    /**
     * @private
     * @type {number}
     */
    this._vol = vol;

    /**
     * @private
     * @type {boolean}
     */
    this._error = error;

  }

  /**  @return {string} */
  get date () {
    return this._date;
  }

  /**  @return {number} */
  get open () {
    return this._open;
  }

  /**  @return {number} */
  get close () {
    return this._close;
  }

  /**  @return {number} */
  get max () {
    return this._max;
  }

  /**  @return {number} */
  get min () {
    return this._min;
  }

  /**  @return {number} */
  get vol () {
    return this._vol;
  }

  /**  @return {boolean} */
  get error () {
    return this._error;
  }

  /** @return {string}  */
  toString () {
    return `${this.date}:${this._open}:${this._close}:` +
           `${this._max}:${this._min}:${this._vol}:${this._error}`;
  }

  /**
   * @param {string} s
   * @return {!Quote}
   */
  static fromString (s) {
    const fs = s.split(":");
    return new Quote(
      fs[0], Number(fs[1]), Number(fs[2]), Number(fs[3]),
      Number(fs[4]), Number(fs[5]), fs[6] === "true"
    );
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Quote}
   */
  static fromJs (serial) {
    return new Quote(
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4],
      serial[5],
      serial[6]
    );
  }
  /*--*/
}
