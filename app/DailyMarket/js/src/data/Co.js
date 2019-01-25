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
    this._lastQ = entry[1].length <= 1 ? null
      : entry[1][entry[1].length - 1][1];
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

  /** @return {number} */
  get profits () {
    return this._lastQ === null || this._stocks === 0 ? 0
      : this._stocks * (this._lastQ - this._price);
  }

  /** @return {number} */
  get risk () {
    return this._lastQ === null || this._stocks === 0 ? 0
      : this._stocks *
        ((this._signal < 0 ? -this._signal : this._lastQ * 0.892407) -
        this._price);
  }

  /** @return {number} */
  get signalRatio () {
    return (this._lastQ === null) ? 0
      : this._signal > 0 ? this._lastQ / this._signal
        : -this._signal / this._lastQ;
  }

  /** @return {number} */
  get dayRatio () {
    return (this._lastQ === null) ? 0
      : (this._lastQ - this._qs[0][1]) / this._qs[0][1];
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

  /**
   * Returns sums of riks and profits
   * @param {!Array<!Co>} cos
   * @return {!Array<number>} [risk, profits]
   */
  static allRiskProfits (cos) {
    let risk = -10000;
    let prof = -10000;
    cos.forEach(co => {
      risk += co.risk;
      prof += co.profits;
    });
    return [risk, prof];
  }
}

