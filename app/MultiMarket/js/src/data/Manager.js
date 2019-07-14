// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Dec from "../dmjs/Dec.js";
import It from "../dmjs/It.js";

/// Format to show parameters
export class ManagerFormat {

  /* .
  _rc_ ManagerFormat : from
    prefix: string
    multiplicator: number
    decimals: number
    suffix: string
  */

  /*--*/
  /**
   * @param {string} prefix
   * @param {number} multiplicator
   * @param {number} decimals
   * @param {string} suffix
   */
  constructor (prefix, multiplicator, decimals, suffix) {

    /**
     * @private
     * @type {string}
     */
    this._prefix = prefix;

    /**
     * @private
     * @type {number}
     */
    this._multiplicator = multiplicator;

    /**
     * @private
     * @type {number}
     */
    this._decimals = decimals;

    /**
     * @private
     * @type {string}
     */
    this._suffix = suffix;

  }

  /**  @return {string} */
  get prefix () {
    return this._prefix;
  }

  /**  @return {number} */
  get multiplicator () {
    return this._multiplicator;
  }

  /**  @return {number} */
  get decimals () {
    return this._decimals;
  }

  /**  @return {string} */
  get suffix () {
    return this._suffix;
  }

  /**
   * @param {!Array<?>} serial
   * @return {!ManagerFormat}
   */
  static fromJs (serial) {
    return new ManagerFormat(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/

}

/** Tuple Name-Maximum-Minimum of a parameter. */
export class ModelMxMn {

  /* .
  _rc_ ModelMxMn : from
    name: string
    max : number
    min : number
  */

  /*--*/
  /**
   * @param {string} name
   * @param {number} max
   * @param {number} min
   */
  constructor (name, max, min) {

    /**
     * @private
     * @type {string}
     */
    this._name = name;

    /**
     * @private
     * @type {number}
     */
    this._max = max;

    /**
     * @private
     * @type {number}
     */
    this._min = min;

  }

  /**  @return {string} */
  get name () {
    return this._name;
  }

  /**  @return {number} */
  get max () {
    return this._max;
  }

  /**  @return {number} */
  get min () {
    return this._min;
  }

  /**
   * @param {!Array<?>} serial
   * @return {!ModelMxMn}
   */
  static fromJs (serial) {
    return new ModelMxMn(
      serial[0],
      serial[1],
      serial[2]
    );
  }
  /*--*/

}

/** Manager entry */
export class ManagerEntry {

  /* .
  _rc_ ManagerEntry : from
    model    : string
    params   : !Array<number>
    paramCfs : !Array<!ModelMxMn>
    paramFmts: !Array<!ManagerFormat>
  */

  /*--*/
  /**
   * @param {string} model
   * @param {!Array<number>} params
   * @param {!Array<!ModelMxMn>} paramCfs
   * @param {!Array<!ManagerFormat>} paramFmts
   */
  constructor (model, params, paramCfs, paramFmts) {

    /**
     * @private
     * @type {string}
     */
    this._model = model;

    /**
     * @private
     * @type {!Array<number>}
     */
    this._params = params;

    /**
     * @private
     * @type {!Array<!ModelMxMn>}
     */
    this._paramCfs = paramCfs;

    /**
     * @private
     * @type {!Array<!ManagerFormat>}
     */
    this._paramFmts = paramFmts;

  }

  /**  @return {string} */
  get model () {
    return this._model;
  }

  /**  @return {!Array<number>} */
  get params () {
    return this._params;
  }

  /**  @return {!Array<!ModelMxMn>} */
  get paramCfs () {
    return this._paramCfs;
  }

  /**  @return {!Array<!ManagerFormat>} */
  get paramFmts () {
    return this._paramFmts;
  }

  /**
   * @param {!Array<?>} serial
   * @return {!ManagerEntry}
   */
  static fromJs (serial) {
    return new ManagerEntry(
      serial[0],
      serial[1],
      serial[2].map(e =>
        ModelMxMn.fromJs(e)
      ),
      serial[3].map(e =>
        ManagerFormat.fromJs(e)
      )
    );
  }
  /*--*/

  /**
   * @param {!ManagerEntry} other
   * @return {boolean}
   */
  eqParams (other) {
    const tps = this.params;
    const ops = other.params;
    if (tps.length !== ops.length) {
      return false;
    }
    return It.range(tps.length).every(ix =>
      new Dec(tps[ix], 7).eqValue(new Dec(ops[ix], 7))
    );
  }

}

/** Manager who matches nicks-models*/
export class Manager {

  /* .
  _rc_ Manager : from
    current   : !ManagerEntry
    # Keys are nicks
    entries : !Map<!ManagerEntry>
  */

  /*--*/
  /**
   * @param {!ManagerEntry} current
   * @param {!Map<string, !ManagerEntry>} entries Keys are nicks
   */
  constructor (current, entries) {

    /**
     * @private
     * @type {!ManagerEntry}
     */
    this._current = current;

    /**
     * @private
     * @type {!Map<string, !ManagerEntry>}
     */
    this._entries = entries;

  }

  /**  @return {!ManagerEntry} */
  get current () {
    return this._current;
  }

  /**  @return {!Map<string, !ManagerEntry>} */
  get entries () {
    return this._entries;
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Manager}
   */
  static fromJs (serial) {
    return new Manager(
      ManagerEntry.fromJs(serial[0]),
      new Map(serial[1].map(kv =>
        [kv[0], ManagerEntry.fromJs(kv[1])]
      ))
    );
  }
  /*--*/

}
