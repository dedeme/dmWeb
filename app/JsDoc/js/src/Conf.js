// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Conf");

Conf = class {
  /**
   * @param {string} path
   * @param {string} lang
   * @param {boolean} showAll
   */
  constructor (path, lang, showAll) {
    /** @private */
    this._path = path;
    /** @private */
    this._lang = lang;
    /** @private */
    this._showAll = showAll;
  }

  /** @return {string} */
  path () {
    return this._path;
  }

  /** @param {string} value */
  setPath (value) {
    this._path = value;
  }

  /** @return {string} */
  lang () {
    return this._lang;
  }

  /** @param {string} value */
  setLang (value) {
    this._lang = value;
  }

  /** @return {boolean} */
  showAll () {
    return this._showAll;
  }

  /** @param {boolean} value */
  setShowAll (value) {
    this._showAll = value;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._path,
      this._lang,
      this._showAll
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Conf}
   */
  static restore (serial) {
    return new Conf (
      serial[0],
      serial[1],
      serial[2]
    );
  }
}
