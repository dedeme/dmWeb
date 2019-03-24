// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


import Main from "../Main.js";
import SysMain from "../sys/SysMain.js";
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

  /**
   * @param {string} id
   * @param {string} tx
   * @param {function()} f
   */
  static mkOption (id, tx, f) {
    return Ui.link(f).att("id", "menu_" + id).text(tx);
  }

  /** Resets menu */
  resetMenu () {
    this._lopts = [];
    this._ropts = [];

    this.addRightMenu(Ui.link(this._main.bye.bind(this._main))
      .add(Ui.img("cross").style("vertical-align:bottom"))
    );
    this.addRightMenu(Dom.separator());
    this.addRightMenu($("span").html("Dormido"));
    this.addRightMenu(Dom.separator());
    this.addRightMenu($("a")
      .att("href", Main.urlBase + "?" + SysMain.homePageId)
      .klass("link")
      .text(_("Home"))
    );
  }

  /**
   * Adds a widget to left part of menu. Direction left to rigth
   * @param {!Domo} wd
   */
  addLeftMenu (wd) {
    this._lopts.push(wd);
  }

  /**
   * Adds a widget to right part of menu. Direction right to left;
   * @param {!Domo} wd
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
