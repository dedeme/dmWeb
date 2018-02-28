// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("index_Tree");

index_Tree = class {
  /**
   * @param {string} id
   * @param {string} help
   * @param {Array<!index_Tree>} entries
   */
  constructor (
    id,
    help,
    entries
  ) {
    /** @private */
    this._id = id;
    /** @private */
    this._help = help;
    /** @private */
    this._entries = entries;
  }

  /** @return {string} */
  id () {
    return this._id;
  }

  /**
   * @private
   * @param {string} value
   */
  setId (value) {
    throw("id is read only");
  }

  /** @return {string} */
  help () {
    return this._help;
  }

  /**
   * @private
   * @param {string} value
   */
  setHelp (value) {
    throw("help is read only");
  }

  /** @return {Array<!index_Tree>} */
  entries () {
    return this._entries;
  }

  /**
   * @private
   * @param {Array<!index_Tree>} value
   */
  setEntries (value) {
    throw("entries is read only");
  }

  /** @return {!Array<?>} */
  serialize () {
    return [
      this._id,
      this._help,
      this._entries === null
        ? null
        : It.from(this._entries).map(e => e.serialize()).to()
    ];
  }

  /**
   * @param {!Array<?>} serial
   * @return {!index_Tree}
   */
  static restore (serial) {
    return new index_Tree (
      serial[0],
      serial[1],
      serial[2] === null
        ? null
        : It.from(serial[2]).map(s => index_Tree.restore(s)).to()
    );
  }
}
