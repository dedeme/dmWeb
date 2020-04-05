// Copyright 08-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Investor model
**/
export default class Fmodel {
  /**
      @param {string} id
      @param {string} name
      @param {!Array<string>} parNames
      @param {!Array<number>} parMins
      @param {!Array<number>} parMaxs
      @param {!Array<string>} parJsFmt
¡
  **/
  constructor (id, name, parNames, parMins, parMaxs, parJsFmt) {
    this._id = id;
    this._name = name;
    this._parNames = parNames;
    this._parMins = parMins;
    this._parMaxs = parMaxs;
    this._parJsFmt = parJsFmt;
  }

  /**
      @return {string}
  **/
  get id () {
    return this._id;
  }

  /**
      @return {string}
  **/
  get name () {
    return this._name;
  }

  /**
      @return {!Array<string>}
  **/
  get parNames () {
    return this._parNames;
  }

  /**
      @return {!Array<number>}
  **/
  get parMaxs () {
    return this._parMaxs;
  }

  /**
      @return {!Array<number>}
  **/
  get parMins () {
    return this._parMins;
  }

  /**
      @return {!Array<string>}
  **/
  get parJsFmt () {
    return this._parJsFmt;
  }

  /**
      @param {!Array<?>} js
      @return {!Fmodel}
  **/
  static fromJs (js) {
    return new Fmodel(js[0], js[1], js[2], js[3], js[4], js[5]);
  }
}
