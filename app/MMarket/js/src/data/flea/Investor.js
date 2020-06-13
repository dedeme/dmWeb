// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Eflea from "./Eflea.js";
import Fmodel from "./Fmodel.js";

/**
    Investor data.
**/
export default class Investor {
  /* .
  _rc_ Investor: from
    model   : Fmodel
    eflea   : !Eflea
  */

  /*--*/
  /**
      @param {Fmodel} model
      @param {!Eflea} eflea
  **/
  constructor (model, eflea) {

    /**
        @private
        @type {Fmodel}
    **/
    this._model = model;

    /**
        @private
        @type {!Eflea}
    **/
    this._eflea = eflea;

  }

  /**
      @return {Fmodel}
  **/
  get model () {
    return this._model;
  }

  /**
      @return {!Eflea}
  **/
  get eflea () {
    return this._eflea;
  }

  /**
      @param {!Array<?>} serial
      @return {!Investor}
  **/
  static fromJs (serial) {
    return new Investor(
      serial[0] ? Fmodel.fromJs(serial[0]) : null,
      Eflea.fromJs(serial[1])
    );
  }
  /*--*/

  /**
      @return {string}
  **/
  get name () {
    return this._model.id + "-" + this._eflea.flea.name;
  }

}
