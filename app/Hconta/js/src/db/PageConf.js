// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Configuration of pages Diary, Cash and Accs */
goog.provide("db_PageConf");

db_PageConf = class {
  /**
   * @param {string} id Account identifier for help or for display (in Accs
   *                    page)
   * @param {number} ix First annotation number of list
   * @param {number} listLen Len of list
   */
  constructor (id, ix, listLen) {
    /** @private */
    this._id = id;
    /** @private */
    this._ix = ix;
    /** @private */
    this._listLen = listLen;
  }

  /** @return {string} */
  id () {
    return this._id;
  }

  /** @param {string} value */
  setId (value) {
    this._id = value;
  }

  /** @return {number} */
  ix () {
    return this._ix;
  }

  /** @param {number} value */
  setIx (value) {
    this._ix = value;
  }

  /** @return {number} */
  listLen () {
    return this._listLen;
  }

  /** @param {number} value */
  setListLen (value) {
    this._listLen = value;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._id,
      this._ix,
      this._listLen
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!db_PageConf}
   */
  static restore (serial) {
    return new db_PageConf (
      serial[0],
      serial[1],
      serial[2]
    );
  }
}
