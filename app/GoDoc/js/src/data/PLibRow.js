// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Cts from "../Cts.js";
import Tp from "../dmjs/Tp.js";

/**
    Personal library entry.
**/

export default class PLibRow {

  /* .
  _rc_ PLibRow : from
  path  : string
  order : number
  pond  : number
  */

  /*--*/
  /**
      @param {string} path
      @param {number} order
      @param {number} pond
  **/
  constructor (path, order, pond) {

    /**
        @private
        @type {string}
    **/
    this._path = path;

    /**
        @private
        @type {number}
    **/
    this._order = order;

    /**
        @private
        @type {number}
    **/
    this._pond = pond;

  }

  /**
      @return {string}
  **/
  get path () {
    return this._path;
  }

  /**
      @return {number}
  **/
  get order () {
    return this._order;
  }

  /**
      @return {number}
  **/
  get pond () {
    return this._pond;
  }

  /**
      @param {!Array<?>} serial
      @return {!PLibRow}
  **/
  static fromJs (serial) {
    return new PLibRow(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

  /**
      Prepares libs to use in web.
      'libs' is alphabeticaly sorted, and returns two new lists: most used and
      menu.
      @param {boolean} isPersonal
      @param {!Array<!PLibRow>} libs
      @return {!Tp<!Array<!PLibRow>, !Array<!PLibRow>>} Tuple of most used
        list and menu list.
  **/
  prepare (isPersonal, libs) {
    libs.sort((e1, e2) => e1.pond < e2.pond ? 1 : -1);
    const lMostUsed = libs.slice(0, Cts.mostUsedEntries);

    const lMenu = libs.filter(e => e.order > 0)
      .sort((e1, e2) => e1.order < e2.order ? 1 : -1)
    ;

    libs.sort((e1, e2) => e1.path > e2.path ? 1 : -1);

    return new Tp(lMostUsed, lMenu);
  }
}
