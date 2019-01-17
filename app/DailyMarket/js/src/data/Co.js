// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Nick data. */
export default class Co {
  /**
   * @private
   * param {!Array<?>} entry
   */
  constructor (entry) {
    this._qs = entry[1];
    this._stocks = entry[2];
    this._price = entry[3];
    this._signal = entry[4];
  }

  /** @return {!Array<!Array<number>>} Array<number> is [hour, quote] */
  get qs () {
    return this._qs;
  }

  /** @return {number} */
  get stocks () {
    return this._stocks;
  }

  /** @return {number} */
  get price () {
    return this._price;
  }

  /**
   * @return {number} (> 0) -> Suport (buying). (< 0) -> Resitence (selling).
   *                  (= 0) -> Not operation.
   */
  get signal () {
    return this._signal;
  }

  /**
   * @param {!Array<?>} d Entries
   * @return {!Map<String, Co>}
   */
  static mkMap (d) {
    const r = new Map();
    d.forEach(e => {
      r.set(e[0], new Co(e));
    });
    return r;
  }

}

