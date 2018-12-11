// Copyright 12-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {_, _args} from "../I18n.js";

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
