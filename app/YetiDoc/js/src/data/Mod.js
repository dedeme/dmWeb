// Copyright 19-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Module data.
**/

/**
    Link
**/
class Link {
  /* .
  _rc_ Link: from
    name : string
    link : number
  */

  /*--*/
  /**
      @param {string} name
      @param {number} link
  **/
  constructor (name, link) {

    /**
        @private
        @type {string}
    **/
    this._name = name;

    /**
        @private
        @type {number}
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
      @return {number}
  **/
  get link () {
    return this._link;
  }

  /**
      @param {!Array<?>} serial
      @return {!Link}
  **/
  static fromJs (serial) {
    return new Link(
      serial[0],
      serial[1]
    );
  }
  /*--*/
}

/**
    Tree entry
**/
class TreeEntry {
  /* .
  _rc_ TreeEntry: from
    tp : string
    enums : !Array<string>
    ms : !Array<string>
    ps : !Array<string>
  */

  /*--*/
  /**
      @param {string} tp
      @param {!Array<string>} enums
      @param {!Array<string>} ms
      @param {!Array<string>} ps
  **/
  constructor (tp, enums, ms, ps) {

    /**
        @private
        @type {string}
    **/
    this._tp = tp;

    /**
        @private
        @type {!Array<string>}
    **/
    this._enums = enums;

    /**
        @private
        @type {!Array<string>}
    **/
    this._ms = ms;

    /**
        @private
        @type {!Array<string>}
    **/
    this._ps = ps;

  }

  /**
      @return {string}
  **/
  get tp () {
    return this._tp;
  }

  /**
      @return {!Array<string>}
  **/
  get enums () {
    return this._enums;
  }

  /**
      @return {!Array<string>}
  **/
  get ms () {
    return this._ms;
  }

  /**
      @return {!Array<string>}
  **/
  get ps () {
    return this._ps;
  }

  /**
      @param {!Array<?>} serial
      @return {!TreeEntry}
  **/
  static fromJs (serial) {
    return new TreeEntry(
      serial[0],
      serial[1],
      serial[2],
      serial[3]
    );
  }
  /*--*/

}

/**
    Module data.
**/
export class Mod {
  /* .
  _rc_ Mod: from
    title : string
    html1 : string
    html2 : string
    link  : !Link
    tree  : !Array<!TreeEntry>
  */

  /*--*/
  /**
      @param {string} title
      @param {string} html1
      @param {string} html2
      @param {!Link} link
      @param {!Array<!TreeEntry>} tree
  **/
  constructor (
    title,
    html1,
    html2,
    link,
    tree
  ) {

    /**
        @private
        @type {string}
    **/
    this._title = title;

    /**
        @private
        @type {string}
    **/
    this._html1 = html1;

    /**
        @private
        @type {string}
    **/
    this._html2 = html2;

    /**
        @private
        @type {!Link}
    **/
    this._link = link;

    /**
        @private
        @type {!Array<!TreeEntry>}
    **/
    this._tree = tree;

  }

  /**
      @return {string}
  **/
  get title () {
    return this._title;
  }

  /**
      @return {string}
  **/
  get html1 () {
    return this._html1;
  }

  /**
      @return {string}
  **/
  get html2 () {
    return this._html2;
  }

  /**
      @return {!Link}
  **/
  get link () {
    return this._link;
  }

  /**
      @return {!Array<!TreeEntry>}
  **/
  get tree () {
    return this._tree;
  }

  /**
      @param {!Array<?>} serial
      @return {!Mod}
  **/
  static fromJs (serial) {
    return new Mod(
      serial[0],
      serial[1],
      serial[2],
      Link.fromJs(serial[3]),
      serial[4].map(e =>
        TreeEntry.fromJs(e)
      )
    );
  }
  /*--*/
}
