// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_TxMethod");


module_TxMethod = class {
  /**
   * @param {boolean} st If this is a static method.
   * @param {string} id
   * @param {string} head
   * @param {string} help
   */
  constructor (
    st,
    id,
    head,
    help
  ) {
    /** @private */
    this._st = st;
    /** @private */
    this._id = id;
    /** @private */
    this._head = head;
    /** @private */
    this._help = help;
  }

  /** @return {boolean} */
  st () {
    return this._st;
  }

  /** @return {string} */
  id () {
    return this._id;
  }

  /** @return {string} */
  head () {
    return this._head;
  }

  /** @return {string} */
  help () {
    return this._help;
  }

  /**
   * @param {!module_TxMethod} m1
   * @param {!module_TxMethod} m2
   * @return {number}
   */
  static sortf (m1, m2) {
    return m1._st
      ? m2._st
        ? m1._id > m2._id
          ? 1
          : -1
        : 1
      : m2._st
        ? -1
        : m1._id > m2._id
          ? 1
          : -1;
  }
}
