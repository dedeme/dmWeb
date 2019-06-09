// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Market open and close hours */
export default class Timetable {

  /* .
  _rc_ Timetable : serial
    hopen  : number
    mopen  : number
    hclose : number
    mclose : number
  */

  /*--*/
  /**
   * @param {number} hopen
   * @param {number} mopen
   * @param {number} hclose
   * @param {number} mclose
   */
  constructor (hopen, mopen, hclose, mclose) {

    /**
     * @private
     * @type {number}
     */
    this._hopen = hopen;

    /**
     * @private
     * @type {number}
     */
    this._mopen = mopen;

    /**
     * @private
     * @type {number}
     */
    this._hclose = hclose;

    /**
     * @private
     * @type {number}
     */
    this._mclose = mclose;

  }

  /**  @return {number} */
  get hopen () {
    return this._hopen;
  }

  /**  @return {number} */
  get mopen () {
    return this._mopen;
  }

  /**  @return {number} */
  get hclose () {
    return this._hclose;
  }

  /**  @return {number} */
  get mclose () {
    return this._mclose;
  }

  /** @return {!Array<?>} */
  toJs () {
    return [
      this._hopen,
      this._mopen,
      this._hclose,
      this._mclose
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Timetable}
   */
  static fromJs (serial) {
    return new Timetable(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/

}
