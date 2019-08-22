// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Module documentation data.
**/

/**
    Module documentation entry.
**/
export class DocEntry {

  /* .
  _rc_ DocEntry: from
    # Entry name.
    name: string
    # Entry documentation.
    doc: string
    # Code documented.
    code: string
    # C code link.
    link: string
  */

  /*--*/
  /**
      @param {string} name Entry name.
      @param {string} doc Entry documentation.
      @param {string} code Code documented.
      @param {string} link C code link.
  **/
  constructor (name, doc, code, link) {

    /**
        @private
        @type {string}
    **/
    this._name = name;

    /**
        @private
        @type {string}
    **/
    this._doc = doc;

    /**
        @private
        @type {string}
    **/
    this._code = code;

    /**
        @private
        @type {string}
    **/
    this._link = link;

  }

  /**
      @return {string}
  **/
  get name () {
    return this._name;
  }

  /**
      @return {string}
  **/
  get doc () {
    return this._doc;
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
  get link () {
    return this._link;
  }

  /**
      @param {!Array<?>} serial
      @return {!DocEntry}
  **/
  static fromJs (serial) {
    return new DocEntry(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/
}

/**
    Module documentation data.
**/
export class Doc {

  /* .
  _rc_ Doc: from
    # Module documentation.
    doc: string
    defines: !Array<!DocEntry>
    enums: !Array<!DocEntry>
    structs: !Array<!DocEntry>
    typedefs: !Array<!DocEntry>
    unions: !Array<!DocEntry>
    functions: !Array<!DocEntry>
    vars: !Array<!DocEntry>
  */

  /*--*/
  /**
      @param {string} doc Module documentation.
      @param {!Array<!DocEntry>} defines
      @param {!Array<!DocEntry>} enums
      @param {!Array<!DocEntry>} structs
      @param {!Array<!DocEntry>} typedefs
      @param {!Array<!DocEntry>} unions
      @param {!Array<!DocEntry>} functions
      @param {!Array<!DocEntry>} vars
  **/
  constructor (
    doc,
    defines,
    enums,
    structs,
    typedefs,
    unions,
    functions,
    vars
  ) {

    /**
        @private
        @type {string}
    **/
    this._doc = doc;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._defines = defines;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._enums = enums;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._structs = structs;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._typedefs = typedefs;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._unions = unions;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._functions = functions;

    /**
        @private
        @type {!Array<!DocEntry>}
    **/
    this._vars = vars;

  }

  /**
      @return {string}
  **/
  get doc () {
    return this._doc;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get defines () {
    return this._defines;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get enums () {
    return this._enums;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get structs () {
    return this._structs;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get typedefs () {
    return this._typedefs;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get unions () {
    return this._unions;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get functions () {
    return this._functions;
  }

  /**
      @return {!Array<!DocEntry>}
  **/
  get vars () {
    return this._vars;
  }

  /**
      @param {!Array<?>} serial
      @return {!Doc}
  **/
  static fromJs (serial) {
    return new Doc(
      serial[0],
      serial[1].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[2].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[3].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[4].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[5].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[6].map(e =>
        DocEntry.fromJs(e)
      ),
      serial[7].map(e =>
        DocEntry.fromJs(e)
      )
    );
  }
  /*--*/

}
