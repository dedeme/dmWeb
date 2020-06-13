// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Manager data.
**/

import Dec from "../dmjs/Dec.js";
import It from "../dmjs/It.js";
import Fmodel from "./flea/Fmodel.js";

/**
    Manager entry
**/
export class ManagerEntry {

  /* .
  _rc_ ManagerEntry : from
    model: !Fmodel
    params: !Array<number>
  */

  /*--*/
  /**
      @param {!Fmodel} model
      @param {!Array<number>} params
  **/
  constructor (model, params) {

    /**
        @private
        @type {!Fmodel}
    **/
    this._model = model;

    /**
        @private
        @type {!Array<number>}
    **/
    this._params = params;

  }

  /**
      @return {!Fmodel}
  **/
  get model () {
    return this._model;
  }

  /**
      @return {!Array<number>}
  **/
  get params () {
    return this._params;
  }

  /**
      @param {!Array<?>} serial
      @return {!ManagerEntry}
  **/
  static fromJs (serial) {
    return new ManagerEntry(
      Fmodel.fromJs(serial[0]),
      serial[1]
    );
  }
  /*--*/

  /**
      @param {!ManagerEntry} other
      @return {boolean}
  **/
  eqParams (other) {
    const tps = this.params;
    const ops = other.params;
    if (tps.length !== ops.length) {
      return false;
    }
    return this.model.id === other.model.id &&
      It.range(tps.length).every(ix =>
        new Dec(tps[ix], 7).eqValue(new Dec(ops[ix], 7))
      )
    ;
  }

}

/**
    Manager data.
**/
export default class Manager {

  /**
      @param {!ManagerEntry} base
      @param {!Object<string, !ManagerEntry>} nicks
  **/
  constructor (base, nicks) {

    /**
        @private
        @type {!ManagerEntry}
    **/
    this._base = base;

    /**
        @private
        @type {!Object<string, !ManagerEntry>}
    **/
    this._nicks = nicks;

  }

  /**
      @return {!ManagerEntry}
  **/
  get base () {
    return this._base;
  }

  /**
      @return {!Object<string, !ManagerEntry>}
  **/
  get nicks () {
    return this._nicks;
  }

  /**
      @param {!Array<?>} serial
      @return {!Manager}
  **/
  static fromJs (serial) {
    return new Manager(
      ManagerEntry.fromJs(serial[0]),
      Object.entries(serial[1]).reduce((r, kv) => {
        r[kv[0]] = ManagerEntry.fromJs(kv[1]);
        return r;
      }, {})
    );
  }
}
