// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Log data. */
export default class Log {
  /**
   * @private
   * param {string} entry
   */
  constructor (entry) {
    const ps = entry.split("|");
    this._isError = ps[0].charAt(0) === "e";
    this._date = ps[0].substring(1);
    this._hour = ps[1];
    this._msg = ps[2];
  }

  /** @return {boolean} */
  get isError () {
    return this._isError;
  }

  /** @return {string} */
  get date () {
    return this._date;
  }

  /** @return {string} */
  get hour () {
    return this._hour;
  }

  /** @return {string} */
  get msg () {
    return this._msg;
  }

  /**
   * @param {!Array<string>} d Entries
   * @return {!Array<!Log>}
   */
  static mk (d) {
    return d.map(e => new Log(e));
  }

}

