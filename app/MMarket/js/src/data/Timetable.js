// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Market open and close hours
**/
export default class Timetable {
  /**
      @param {number} hopen
      @param {number} mopen
      @param {number} hclose
      @param {number} mclose
   */
  constructor (hopen, mopen, hclose, mclose) {
    this._hopen = hopen;
    this._mopen = mopen;
    this._hclose = hclose;
    this._mclose = mclose;
  }

  /**
      @return {number}
  **/
  get hopen () {
    return this._hopen;
  }

  /**
      @return {number}
  **/
  get mopen () {
    return this._mopen;
  }

  /**
      @return {number}
  **/
  get hclose () {
    return this._hclose;
  }

  /**
      @return {number}
  **/
  get mclose () {
    return this._mclose;
  }

  /**
      @return {!Array<?>}
  **/
  toJs () {
    return [
      this._hopen,
      this._mopen,
      this._hclose,
      this._mclose
    ];
  }

  /**
      @param {!Array<?>} serial
      @return {!Timetable}
  **/
  static fromJs (serial) {
    return new Timetable(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
}
