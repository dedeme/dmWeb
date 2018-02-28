// Copyright 19-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Family");

Family = class {
  /**
   * @param {number} fields
   * @param {function(function(number):string,function(number):string,number):!It<!Domo>} bests
   * @param {function(function(number):string,function(number):string,!It<!Domo>,!It<!Domo>):!It<!Domo>} trace
   * @param {function(!Object<string, !Array<!Quote>>,!Trace,boolean):boolean} traceError
   * @param {function(!Object<string, !Array<!Quote>>,!Trace):!It<!Domo>} traceBody
   */
  constructor (fields, bests, trace, traceError, traceBody) {
    /** @private */
    this._fields = fields;
    /** @private */
    this._bests = bests;
    /** @private */
    this._trace = trace;
    /** @private */
    this._traceError = traceError;
    /** @private */
    this._traceBody = traceBody;
  }

  /** @return {number} */
  fields () {
    return this._fields;
  }

  /** @return {function(function(number):string,function(number):string,number):!It<!Domo>} */
  bests () {
    return this._bests;
  }

  /** @return {function(function(number):string,function(number):string,!It<!Domo>,!It<!Domo>):!It<!Domo>} */
  trace () {
    return this._trace;
  }

  /** @return {function(!Object<string, !Array<!Quote>>,!Trace,boolean):boolean} */
  traceError () {
    return this._traceError;
  }

  /** @return {function(!Object<string, !Array<!Quote>>,!Trace):!It<!Domo>} */
  traceBody () {
    return this._traceBody;
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._fields,
      this._bests,
      this._trace,
      this._traceError,
      this._traceBody
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!Family}
   */
  static restore (serial) {
    return new Family (
      serial[0],
      serial[1],
      serial[2],
      serial[3],
      serial[4]
    );
  }
}
