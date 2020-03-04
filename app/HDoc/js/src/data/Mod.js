// Copyright 03-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Module data.
**/

/**
    Module entry.
**/
export class ModEntry {
  /**
      @param {number} line Line number of definition. For overview is 0.
      @param {string} id Identifier. For overview is "".
      @param {string} code Code definition. For overview is "".
      @param {string} doc Documentation. Verbatin text, removing '--- '.
                          Its value can be "".
      @param {!Array<string>} instances But for classes it is an empty Array.
                                         For classes also can be empty.
  **/
  constructor (line, id, code, doc, instances) {
    this._line = line;
    this._id = id;
    this._code = code;
    this._doc = doc;
    this._instances = instances;
  }

  /**
      @return {number}
  **/
  get line () {
    return this._line;
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
  get code () {
    return this._code;
  }

  /**
      @return {string}
  **/
  get doc () {
    return this._doc;
  }

  /**
      @return {!Array<string>}
  **/
  get instances () {
    return this._instances;
  }

}

export class Mod {

  constructor () {
    /**
        @type {!Array<!ModEntry>}
    **/
    this._overview = [];

    /**
        @type {!Array<!ModEntry>}
    **/
    this._entries = [];
  }

  /**
      @return {!Array<!ModEntry> }
  **/
  get overview () {
    return this._overview;
  }

  /**
      @return {!Array<!ModEntry> }
  **/
  get entries () {
    return this._entries;
  }

}
