// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Source tree.
**/

export default class STree {
  /**
      @param {boolean} isDir
      @param {string} name
      @param {string | !Array<STree>} data
  **/
  constructor (isDir, name, data) {

    /**
        @private
        @type {boolean}
    **/
    this._isDir = isDir;

    /**
        @private
        @type {string}
    **/
    this._name = name;

    /**
        @private
        @type {string | !Array<STree>}
    **/
    this._data = data;

  }

  /**
      @return {boolean}
  **/
  get isDir () {
    return this._isDir;
  }

  /**
      @return {string}
  **/
  get name () {
    return this._name;
  }

  /**
      @return {string | !Array<STree>}
  **/
  get data () {
    return this._data;
  }

  /**
      Sorts this.
  **/
  sort () {
    if (this._isDir) {
      const dt = /** @type {!Array<STree>} */ (this._data);
      dt.sort((e1, e2) =>
        e1.isDir
          ? e2.isDir
            ? e1.name.toUpperCase() > e2.name.toUpperCase() ? 1 : -1
            : 1
          : e2.isDir
            ? -2
            : e1.name.toUpperCase() > e2.name.toUpperCase() ? 1 : -1
      );
      dt.forEach(e => { e.sort() });
    }
  }

  /**
      @param {!Array<?>} serial
      @return {!STree}
  **/
  static fromJs (serial) {
    return new STree(
      serial[0],
      serial[1],
      serial[0]
        ? serial[2].map(js => STree.fromJs(/** @type {!Array<?>} */ (js)))
        : serial[2]
    );
  }
}
