// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Diary entry */
goog.provide("db_Dentry");

db_Dentry = class {
  /**
   * @param {!DateDm} date
   * @param {string} description
   * @param {!Array<!Tp<string,!Dec>>} debits
   * @param {!Array<!Tp<string,!Dec>>} credits
   */
  constructor (date, description, debits, credits) {
    /** @private */
    this._date = date;
    /** @private */
    this._description = description;
    /** @private */
    this._debits = debits;
    /** @private */
    this._credits = credits;
  }

  /** @return {!DateDm} */
  date () {
    return this._date;
  }

  /** @return {string} */
  description () {
    return this._description;
  }

  /** @return {!Array<!Tp<string,!Dec>>} */
  debits () {
    return this._debits;
  }

  /** @return {!Array<!Tp<string,!Dec>>} */
  credits () {
    return this._credits;
  }

  /**
   * Returns 'true' if 'this' contains acc in debits or credits
   * @param {string} acc
   * @return {boolean}
   */
  containsAccount (acc) {
    return It.from(this._debits).containsf(tp => tp.e1() === acc) ||
      It.from(this._credits).containsf(tp => tp.e1() === acc);
  }

  /**
   * Returns 'true' if this contains acc or is group in debits or credits
   * @param {string} acc
   * @return {boolean}
   */
  containsAccountOrGroup (acc) {
    return It.from(this._debits).containsf(tp => tp.e1().startsWith(acc)) ||
      It.from(this._credits).containsf(tp => tp.e1().startsWith(acc));
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._date.serialize(),
      this._description,
      It.from(this._debits).map(tp => [tp.e1(), tp.e2().serialize()]).to(),
      It.from(this._credits).map(tp => [tp.e1(), tp.e2().serialize()]).to()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!db_Dentry}
   */
  static restore (serial) {
    return new db_Dentry(
      DateDm.restore(serial[0]),
      serial[1],
      It.from(serial[2]).map(arr => new Tp(arr[0], Dec.restore(arr[1]))).to(),
      It.from(serial[3]).map(arr => new Tp(arr[0], Dec.restore(arr[1]))).to()
    );
  }
}
