// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


/**
    Configuration data.
**/
export default class Conf {

  /* .
  _rc_ Conf: serial
    path: string
    lang: string
    showAll: boolean
  */

  /*--*/
  /**
      @param {string} path
      @param {string} lang
      @param {boolean} showAll
  **/
  constructor (path, lang, showAll) {

    /**
        @private
        @type {string}
    **/
    this._path = path;

    /**
        @private
        @type {string}
    **/
    this._lang = lang;

    /**
        @private
        @type {boolean}
    **/
    this._showAll = showAll;

  }

  /**
      @return {string}
  **/
  get path () {
    return this._path;
  }

  /**
      @return {string}
  **/
  get lang () {
    return this._lang;
  }

  /**
      @return {boolean}
  **/
  get showAll () {
    return this._showAll;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._path,
      this._lang,
      this._showAll
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Conf}
  **/
  static fromJs (serial) {
    return new Conf(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/
}
