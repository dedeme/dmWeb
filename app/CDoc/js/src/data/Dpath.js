// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Documentation path data.
**/
export default class Dpath {

  /* .
  _rc_ Dpath: serial
    id: string
    path: string
    show: boolean
    valid: boolean
  */

  /*--*/
  /**
      @param {string} id
      @param {string} path
      @param {boolean} show
      @param {boolean} valid
  **/
  constructor (id, path, show, valid) {

    /**
        @private
        @type {string}
    **/
    this._id = id;

    /**
        @private
        @type {string}
    **/
    this._path = path;

    /**
        @private
        @type {boolean}
    **/
    this._show = show;

    /**
        @private
        @type {boolean}
    **/
    this._valid = valid;

  }

  /**
      @return {string}
  **/
  get id () {
    return this._id;
  }

  /**
      @return {string}
  **/
  get path () {
    return this._path;
  }

  /**
      @return {boolean}
  **/
  get show () {
    return this._show;
  }

  /**
      @return {boolean}
  **/
  get valid () {
    return this._valid;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._id,
      this._path,
      this._show,
      this._valid
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Dpath}
  **/
  static fromJs (serial) {
    return new Dpath(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/
}
