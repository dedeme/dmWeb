// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Dpaths index tree.
**/
export default class IndexTree {

  /* .
  _rc_ IndexTree: from
    id: string
    isPath: boolean
    # If isPath is 'false', 'doc' is an empty string.
    doc: string
    # If isPath is 'true', 'tree' is 'null'.
    trees: !Array<!IndexTree>
  */

  /*--*/
  /**
      @param {string} id
      @param {boolean} isPath
      @param {string} doc If isPath is 'false', 'doc' is an empty string.
      @param {!Array<!IndexTree>} trees If isPath is 'true', 'tree' is 'null'.
  **/
  constructor (id, isPath, doc, trees) {

    /**
        @private
        @type {string}
    **/
    this._id = id;

    /**
        @private
        @type {boolean}
    **/
    this._isPath = isPath;

    /**
        @private
        @type {string}
    **/
    this._doc = doc;

    /**
        @private
        @type {!Array<!IndexTree>}
    **/
    this._trees = trees;

  }

  /**
      @return {string}
  **/
  get id () {
    return this._id;
  }

  /**
      @return {boolean}
  **/
  get isPath () {
    return this._isPath;
  }

  /**
      @return {string}
  **/
  get doc () {
    return this._doc;
  }

  /**
      @return {!Array<!IndexTree>}
  **/
  get trees () {
    return this._trees;
  }

  /**
      @param {!Array<?>} serial
      @return {!IndexTree}
  **/
  static fromJs (serial) {
    return new IndexTree(
      serial[0],
      serial[1],
      serial[2],
      serial[3].map(e =>
        IndexTree.fromJs(e)
      )
    );
  }
  /*--*/

}
