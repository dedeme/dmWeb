// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Flea from "./Flea.js";

/**
    Evaluated flea data.
**/
export default class Eflea {
  /* .
  _rc_ Eflea: serial
    flea   : !Flea
    buys   : number
    sells  : number
    assets : number
    profits: number
    ev     : number
  */

  /*--*/
  /**
      @param {!Flea} flea
      @param {number} buys
      @param {number} sells
      @param {number} assets
      @param {number} profits
      @param {number} ev
  **/
  constructor (
    flea,
    buys,
    sells,
    assets,
    profits,
    ev
  ) {

    /**
        @private
        @type {!Flea}
    **/
    this._flea = flea;

    /**
        @private
        @type {number}
    **/
    this._buys = buys;

    /**
        @private
        @type {number}
    **/
    this._sells = sells;

    /**
        @private
        @type {number}
    **/
    this._assets = assets;

    /**
        @private
        @type {number}
    **/
    this._profits = profits;

    /**
        @private
        @type {number}
    **/
    this._ev = ev;

  }

  /**
      @return {!Flea}
  **/
  get flea () {
    return this._flea;
  }

  /**
      @return {number}
  **/
  get buys () {
    return this._buys;
  }

  /**
      @return {number}
  **/
  get sells () {
    return this._sells;
  }

  /**
      @return {number}
  **/
  get assets () {
    return this._assets;
  }

  /**
      @return {number}
  **/
  get profits () {
    return this._profits;
  }

  /**
      @return {number}
  **/
  get ev () {
    return this._ev;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._flea.toJs(),
      this._buys,
      this._sells,
      this._assets,
      this._profits,
      this._ev
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Eflea}
  **/
  static fromJs (serial) {
    return new Eflea(
      Flea.fromJs(serial[0]),
      serial[1],
      serial[2],
      serial[3],
      serial[4],
      serial[5]
    );
  }
  /*--*/
}
