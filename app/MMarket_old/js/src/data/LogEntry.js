// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Log entry
**/

import DateDm from "../dmjs/DateDm.js";

/**
    @private
    @param {string} m
    @param {number} indent
    @param {number} len
    @return {string}
**/
function format (m, indent, len) {
  if (m.trim() === "") return m;

  const r = [];
  m.split("\n").forEach(l => {
    const subr = [];

    while (l.length > len) {
      let line = l.substring(0, len);
      l = l.substring(len);
      const ix = line.lastIndexOf(" ");
      if (ix !== -1 && line.substring(0, ix).trim() !== "") {
        l = line.substring(ix + 1) + l;
        line = line.substring(0, ix);
      }
      subr.push(line);
    }

    if (l.trim() !== "") subr.push(l);
    subr.forEach(subl => { r.push(subl) });
  });

  const ind = " ".repeat(indent);
  return r.join("\n" + ind);
}

/**
    Log entry
**/
export default class LogEntry {
  /**
      @private
      @param {boolean} isError
      @param {string} time
      @param {string} msg
  **/
  constructor (isError, time, msg) {
    this._isError = isError;
    this._time = time;
    this._msg = msg;
  }

  /**
      @return {boolean}
  **/
  get isError () {
    return this._isError;
  }

  /**
      @return {!DateDm}
  **/
  get date () {
    const ix = this._time.indexOf("(");
    return DateDm.fromIso(this._time.substring(0, ix).trim());
  }

  /**
      @return {string}
  **/
  toString () {
    const t = this._time;
    const indent = t.length + 3;
    const len = LogEntry.lineWidth() - indent;
    const sep = this._isError ? " = " : " - ";
    return t + sep + format(this._msg, indent, len);
  }

  /**
      @param {!Array<?>} a
      @return {!LogEntry}
  **/
  static fromJs (a) {
    return new LogEntry(a[0], a[1], a[2]);
  }

  /**
      Number of caracters per line in function 'toString()'
      @return {number}
  **/
  static lineWidth () {
    return 120;
  }

}
