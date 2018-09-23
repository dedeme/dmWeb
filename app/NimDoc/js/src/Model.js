// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Main page model and MenuPath class. */

/** Path of librery */
export class MenuPath {
  /**
   * @param {string} id Identifier
   * @param {string} path Path
   * @param {boolean} show If should be shown
   * @param {boolean} ok If path exists
   */
  constructor (id, path, show, ok) {
    this._id = id;
    this._path = path;
    this._show = show;
    this._ok = ok;
  }

  /** @return {string} Id */
  get id () {
    return this._id;
  }

  /** @return {string} Path */
  get path () {
    return this._path;
  }

  /** @return {boolean} Show */
  get show () {
    return this._show;
  }

  /** @return {boolean} Ok */
  get ok () {
    return this._ok;
  }
}

/** Main model */
export class Model {

  /**
   * @param {string} locationPath Path of browser
   * @param {string} lang Language
   * @param {boolean} showAll If every MenuPath is shown
   * @param {!Array<MenuPath>} paths All the paths
   */
  constructor (locationPath, lang, showAll, paths) {
    this._locationPath = locationPath;
    this._lang = lang;
    this._showAll = showAll;
    this._paths = paths;
    this._sel = "@";
    this._module = "";
    this._link = "";
  }

  /** @return {string} LocationPath */
  get locationPath () {
    return this._locationPath;
  }

  /** @return {string} Language */
  get lang () {
    return this._lang;
  }

  /** @return {boolean} ShowAll */
  get showAll () {
    return this._showAll;
  }

  /** @return {!Array<MenuPath>} Paths */
  get paths () {
    return this._paths;
  }

  /** @return {string} Selected path id */
  get sel () {
    return this._sel;
  }

  /**
   * Selects a path id.
   * @param {string} id Path id
   * @return {void}
   */
  set sel (id) {
    this._sel = id;
  }

  /** @return {string} Selected module id */
  get module () {
    return this._module;
  }

  /**
   * Selects a module id.
   * @param {string} id Module id
   * @return {void}
   */
  set module (id) {
    this._module = id;
  }

  /** @return {string} Code link */
  get link () {
    return this._link;
  }

  /**
   * Sets code link.
   * @param {string} lk Link
   * @return {void}
   */
  set link (lk) {
    this._link = lk;
  }
}
