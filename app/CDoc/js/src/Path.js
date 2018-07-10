// Copyright 2-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Path");

Path = class {
  /**
   * @param {string} id
   * @param {string} path
   * @param {boolean} show
   * @param {boolean} valid
   */
  constructor (id, path, show, valid) {
    /** @private */
    this._id = id;
    /** @private */
    this._path = path;
    /** @private */
    this._show = show;
    /** @private */
    this._valid = valid;
  }

  /** @return {string} */
  id () {
    return this._id;
  }

  /** @param {string} value */
  setId (value) {
    this._id = value;
  }

  /** @return {string} */
  path () {
    return this._path;
  }

  /** @param {string} value */
  setPath (value) {
    this._path = value;
  }

  /** @return {boolean} */
  show () {
    return this._show;
  }

  /** @param {boolean} value */
  setShow (value) {
    this._show = value;
  }

  /** @return {boolean} */
  valid () {
    return this._valid;
  }

  /** @param {boolean} value */
  setValid (value) {
    this._valid = value;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._id,
      this._path,
      this._show
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Path}
   */
  static restore (serial) {
    return new Path (
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
}
