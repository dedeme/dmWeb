// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Conf");

goog.require("db_PageConf");

Conf = class {
  /**
   * @param {string} language
   * @param {!Array<number>} years
   * @param {number} year
   * @param {string} page
   * @param {string} summary Last selected option on summaries
   * @param {string} planId Last group/account selected
   * @param {db_PageConf} diaryConf
   * @param {db_PageConf} cashConf
   * @param {db_PageConf} accsConf
   */
  constructor (
    language,
    years,
    year,
    page,
    summary,
    planId,
    diaryConf,
    cashConf,
    accsConf
  ) {
    /** @private */
    this._language = language;
    /** @private */
    this._years = years;
    /** @private */
    this._year = year;
    /** @private */
    this._page = page;
    /** @private */
    this._summary = summary;
    /** @private */
    this._planId = planId;
    /** @private */
    this._diaryConf = diaryConf;
    /** @private */
    this._cashConf = cashConf;
    /** @private */
    this._accsConf = accsConf;
  }

  /** @return {string} */
  language () {
    return this._language;
  }

  /** @param {string} value */
  setLanguage (value) {
    this._language = value;
  }

  /** @return {!Array<number>} A sorted array */
  years () {
    return It.from(this._years).sort().to();
  }

  /** @param {!Array<number>} value */
  setYears (value) {
    this._years = value;
  }

  /** @return {number} */
  year () {
    return this._year;
  }

  /** @param {number} value */
  setYear (value) {
    this._year = value;
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
  summary () {
    return this._summary;
  }

  /** @param {string} value */
  setSummary (value) {
    this._summary = value;
  }

  /** @return {string} */
  planId () {
    return this._planId;
  }

  /** @param {string} value */
  setPlanId (value) {
    this._planId = value;
  }

  /** @return {db_PageConf} */
  diaryConf () {
    return this._diaryConf;
  }

  /** @return {db_PageConf} */
  cashConf () {
    return this._cashConf;
  }

  /** @return {db_PageConf} */
  accsConf () {
    return this._accsConf;
  }

  /** @return {boolean} */
  isLastYear () {
    const years = this.years();
    return this._year === years[years.length - 1];
  }

  /** @return {string} */
  serialize () {
    return JSON.stringify([
      this._language,
      this._years,
      this._year,
      this._page,
      this._summary,
      this._planId,
      this._diaryConf.serialize(),
      this._cashConf.serialize(),
      this._accsConf.serialize()
    ]);
  }

  /**
   * @param {string} serial
   * @return {!Conf}
   */
  static restore (serial) {
    if (serial === "") {
      const year = DateDm.now().year();
      return new Conf("es", [year], year, "settings", "0A", "",
        new db_PageConf("572", 0, 20),
        new db_PageConf("572", 0, 20),
        new db_PageConf("", 0, 20)
      );
    }
    const pars = /** @type {!Array<?>} */(JSON.parse(serial));
    return new Conf (
      pars[0],
      pars[1],
      pars[2],
      pars[3],
      pars[4],
      pars[5],
      db_PageConf.restore(pars[6]),
      db_PageConf.restore(pars[7]),
      db_PageConf.restore( pars[8])
    );
  }
}
