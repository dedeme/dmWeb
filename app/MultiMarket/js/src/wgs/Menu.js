// Copyright 13-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";

const $ = Ui.$;

/** Menu widget. */
export default class Menu {
  constructor () {
    // MODEL -------
    // TTTTTTTTTTTTT

    /**
     * @private
     * @type {string}
     */
    this._selected = "";

    // VIEW --------
    // TTTTTTTTTTTTT

    /**
     * @private
     * @type {!Array<!Domo>}
     */
    this._lopts = [];

    /**
     * @private
     * @type {!Array<!Domo>}
     */
    this._ropts = [];

    /**
     * @private
     * @type {!Domo}
     */
    this._lview = $("div");

    /**
     * @private
     * @type {!Domo}
     */
    this._rview = $("div");
  }

  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    return $("div")
      .add($("table").klass("main").add($("tr")
        .add($("td").add(this._lview))
        .add($("td").style("text-align:right").add(this._rview))))
      .add($("hr"))
    ;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * Removes all items from menu
   * @return {void}
   */
  reset () {
    this._lopts = [];
    this._lview.removeAll();
    this._ropts = [];
    this._rview.removeAll();
  }

  /**
   * Adds elements to left part of menu. Elements are added left to right.
   * @param {!Domo} o
   * @return {void}
   */
  addLeft (o) {
    this._lopts.push(o);
    this._lview.removeAll().adds(this._lopts);
  }

  /**
   * Adds elements to right part of menu. Elements are added right to left.
   * @param {!Domo} o
   * @return {void}
   */
  addRight (o) {
    this._ropts.unshift(o);
    this._rview.removeAll().adds(this._ropts);
  }

  /**
   * Marks an option as selected.
   * @param {string} id
   * @return {void}
   */
  setSelected (id) {
    const mId = "menu_" + id;
    function setId (o) {
      const att = o.att("id");
      if (att && att !== "") {
        o.klass(att === mId ? "frame" : "link");
      }
    }
    this._lopts.forEach(o => setId(o));
    this._ropts.forEach(o => setId(o));
  }

  // STATIC --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  static separator () {
    return $("span").text(" · ");
  }

  /** @return {!Domo} */
  static separator2 () {
    return $("span").text(" | ");
  }

  /**
   * @param {string} id
   * @param {string} tx
   * @param {function():(void|!Promise)} f
   */
  static mkOption (id, tx, f) {
    return Ui.link(f).att("id", "menu_" + id).text(tx);
  }

  /**
   * Link is formed 'Main.urlBase + "?" + module + "&" + id'
   * @param {string} id
   * @param {string} tx
   * @param {string} module
   * @return {!Domo}
   */
  static mkLink (id, tx, module) {
    return $("a")
      .att("href", Main.urlBase + "?" + module + "&" + id)
      .att("id", id)
      .text(tx)
    ;
  }

  /**
   * @param {function ():Promise} fbye
   * @return {!Domo}
   */
  static mkClose (fbye) {
    return Ui.link(fbye).add(Ui.img("cross").style("vertical-align:bottom"));
  }

}