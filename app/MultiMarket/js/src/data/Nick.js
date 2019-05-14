// Copyright 12-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Nick */
export default class Nick {

  /* .
  _rc_ Nick : from
  id : number
  name : string
  isSel : boolean
  */

  /*--*/
  /**
   * @param {number} id
   * @param {string} name
   * @param {boolean} isSel
   */
  constructor (id, name, isSel) {

    /**
     * @private
     * @type {number}
     */
    this._id = id;

    /**
     * @private
     * @type {string}
     */
    this._name = name;

    /**
     * @private
     * @type {boolean}
     */
    this._isSel = isSel;

  }

  /**  @return {number} */
  get id () {
    return this._id;
  }

  /**  @return {string} */
  get name () {
    return this._name;
  }

  /**  @return {boolean} */
  get isSel () {
    return this._isSel;
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Nick}
   */
  static fromJs (serial) {
    return new Nick(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

}
