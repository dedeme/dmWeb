// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


import Main from "../Main.js";
import {_} from "../I18n.js";
// eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;

/** Container for pages. */
export default class Dom {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

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
  }

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
   * Resets menu
   * @return {void}
   */
  resetMenu () {
    this._lopts = [];
    this._ropts = [];
  }

  /** @return {!Domo} */
  mkClose () {
    return Ui.link(this._main.bye.bind(this._main))
      .add(Ui.img("cross").style("vertical-align:bottom"))
    ;
  }

  /**
   * Adds a widget to left part of menu. Direction left to rigth
   * @param {!Domo} wd
   * @return void
   */
  addLeftMenu (wd) {
    this._lopts.push(wd);
  }

  /**
   * Adds a widget to right part of menu. Direction right to left;
   * @param {!Domo} wd
   * @return void
   */
  addRightMenu (wd) {
    this._ropts.push(wd);
  }

  /**
   * @param {!Domo} o DOM object to show
   * @return {void}
   */
  showRoot (o) {
    $("@body").removeAll().add(
      $("div")
        .add(o)
        .add($("p").html("&nbsp;"))
        .add($("hr"))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td")
              .add($("a")
                .att("href", "doc/about.html")
                .att("target", "blank")
                .html("<small>" + _("Help & Credits") + "</small>")))
            .add($("td")
              .style("text-align: right;font-size: 10px;" +
                "color:#808080;font-size:x-small;")
              .html(`- © ºDeme. ${Main.app} (${Main.version}) -`))))
    );
  }

  /**
   * @param {string} page Page identifier.
   * @param {!Domo} o Page to show inside Dom.
   * @return {void}
   */
  show (page, o) {
    const pageId = "menu_" + page;

    function setClass (wg) {
      const id = wg.att("id");
      if (id) {
        if (id === pageId) {
          wg.klass("frame");
        } else {
          wg.klass("link");
        }
      }
    }

    this._lopts.forEach(wg => setClass(wg));
    this._ropts.forEach(wg => setClass(wg));

    const ropts = this._ropts.map(e => e);
    ropts.reverse();

    const menu = $("table").klass("main").add($("tr")
      .add($("td")
        .adds(this._lopts))
      .add($("td").style("text-align:right")
        .adds(ropts))
    );

    this.showRoot(
      $("div")
        .add(menu)
        .add($("hr"))
        .add(o)
    );
  }
}
