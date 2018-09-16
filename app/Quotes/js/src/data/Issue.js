// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Issue */
export default class Issue {
  constructor (nickId, type, cause) {
    this._nickId = nickId;
    this._type = type;
    this._cause = cause;
  }

  /** @return {string} */
  get nickId () {
    return this._nickId;
  }

  /** @return {number} */
  get type () {
    return this._type;
  }

  /** @return {string} */
  get cause () {
    return this._cause;
  }

  /**
   * There is no issue
   * @return {number}
   */
  static get NONE () {
    return 0;
  }

  /**
   * Nick_id does not exists
   * @return {number}
   */
  static get EXISTS () {
    return 1;
  }

  /**
   * Code of server is missing
   * @return {number}
   */
  static get SERVER () {
    return 2;
  }

  /**
   * Not data at all
   * @return {number}
   */
  static get EMPTY () {
    return 3;
  }

  /**
   * Missing data of a date
   * @return {number}
   */
  static get MISSING () {
    return 4;
  }

  /**
   * Missing value in a field
   * @return {number}
   */
  static get FIELD_MISSING () {
    return 5;
  }

  /**
   * Current quote varies +- 20%
   * @return {number}
   */
  static get BEFORE_NOW () {
    return 6;
  }

  /**
   * Open or Close > Max
   * @return {number}
   */
  static get MAX () {
    return 7;
  }

  /**
   * Open or Close < Min
   * @return {number}
   */
  static get MIN () {
    return 8;
  }

  static fromJson (serial) {
    return new Issue(
      serial[0],
      serial[1],
      serial[2]
    );
  }
}
