// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Application path entry.
**/

export default class PathEntry {

  /* .
  _rc_ PathEntry : from
  name : string
  path : string
  selected : boolean
  exists: boolean
  */

  /*--*/
  /**
      @param {string} name
      @param {string} path
      @param {boolean} selected
      @param {boolean} exists
  **/
  constructor (name, path, selected, exists) {

    /**
        @private
        @type {string}
    **/
    this._name = name;

    /**
        @private
        @type {string}
    **/
    this._path = path;

    /**
        @private
        @type {boolean}
    **/
    this._selected = selected;

    /**
        @private
        @type {boolean}
    **/
    this._exists = exists;

  }

  /**
      @return {string}
  **/
  get name () {
    return this._name;
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
  get selected () {
    return this._selected;
  }

  /**
      @return {boolean}
  **/
  get exists () {
    return this._exists;
  }

  /**
      @param {!Array<?>} serial
      @return {!PathEntry}
  **/
  static fromJs (serial) {
    return new PathEntry(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/
}
