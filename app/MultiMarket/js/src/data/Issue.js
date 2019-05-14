// Copyright 12-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {_, _args} from "../I18n.js";

/** Issue */
export default class Issue {

  // CONSTRUCTOR ---------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @param {string} nickId
   * @param {number} type Enumeration with the fowllowing values:
   *                 Issue.NONE
   *                 Issue.EXISTS
   *                 Issue.SERVER
   *                 Issue.EMPTY
   *                 Issue.MISSING
   *                 Issue.EXTRA
   *                 Issue.BEFORE_NOW
   *                 Issue.MAX
   *                 Issue.MIN
   * @param {string} cause
   */
  constructor (nickId, type, cause) {
    /** @private */
    this._nickId = nickId;
    /** @private */
    this._type = type;
    /** @private */
    this._cause = cause;
  }

  // FIELDS --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

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

  /** @return {string} */
  get msg () {
    const [date, field] = this.cause.split(":");
    switch (this._type) {
    case Issue.SERVER:
      return _args(_("Server code of %0 is missing"), this.cause);
    case Issue.EMPTY:
      return _args(_("File %0 empty or not found"), this.cause);
    case Issue.MISSING:
      return _args(_("Quote of %0 is missimg"), date);
    case Issue.EXTRA:
      return _args(_("Extra quote in %0"), date);
    case Issue.BEFORE_NOW:
      return _args(_("%0: Difference +- 20% in %1"), field, date);
    case Issue.MAX:
      return _args(_("%0 is greater than 'max' in %1"), field, date);
    case Issue.MIN:
      return _args(_("%0 is less than 'min' in %1"), field, date);
    default:
      return "";
    }
  }

  // VARIABLES -----------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


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
   * Missing quote
   * @return {number}
   */
  static get MISSING () {
    return 4;
  }

  /**
   * Extra quote
   * @return {number}
   */
  static get EXTRA () {
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
   * Open or Close &gt; Max
   * @return {number}
   */
  static get MAX () {
    return 7;
  }

  /**
   * Open or Close &lt; Min
   * @return {number}
   */
  static get MIN () {
    return 8;
  }

  // FUNCTIONS -----------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @param {!Array<string|number>} serial
   * @return {!Issue}
   */
  static fromJson (serial) {
    return new Issue(
      /** @type{string} */ (serial[0]),
      /** @type{number} */ (serial[1]),
      /** @type{string} */ (serial[2])
    );
  }
}
