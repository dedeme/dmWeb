// Copyright 07-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Conf");

Conf = class {
  /**
   * @param {string} lang
   * @param {string} page
   * @param {string} subpage
   */
  constructor (lang, page, subpage) {
    /** @private */
    this._lang = lang;
    /** @private */
    this._page = page;
    /** @private */
    this._subpage = subpage;
  }

  /** @return {string} */
  lang () {
    return this._lang;
  }

  /** @param {string} value */
  setLang (value) {
    this._lang = value;
  }

  /** @return {string} */
  page () {
    return this._page;
  }

  /** @param {string} value */
  setPage (value) {
    this._page = value;
  }

  /** @return {string} */
  subpage () {
    return this._subpage;
  }

  /** @param {string} value */
  setSubpage (value) {
    this._subpage = value;
  }

  /**
   * Decimal format
   * @param {!Dec} d
   * @return {string}
   */
  fDec (d) {
    return this._lang === "es" ? d.toEu() : d.toEn();
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._lang,
      this._page,
      this._subpage
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Conf}
   */
  static restore (serial) {
    if (serial.length === 0) {
      return new Conf(
        "es",
        "settings",
        ""
      );
    }
    return new Conf(
      serial[0],
      serial[1],
      serial[2]
    );
  }
}
