// Copyright 12-Dic-2018 ºDeme
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
   * @param {string} page Page name.
   * @param {!Domo} o Page to show inside Dom.
   * @return {void}
   */
  show (page, o) {
    const main = this._main;

    function entry (id, target) {
      return Ui.link(() => {
        main.go(target);
      })
        .klass(target === page ? "frame" : "link").html(id);
    }
    function separator () {
      return $("span").html(" · ");
    }

    const lopts = [
      entry(_("Balance"), Main.balancePageId),
      separator(),
      entry(_("Annotations"), Main.annotationsPageId),
      separator(),
      entry(_("Trading"), Main.tradingPageId),
      separator(),
      entry(_("Profits"), Main.profitsPageId),
      separator(),
      entry(_("Companies"), Main.companiesPageId),
    ];

    const ropts = [
      entry(_("Backs"), Main.backupsPageId),
      separator(),
      entry(_("Settings"), Main.settingsPageId),
      separator(),
      Ui.link(() => {
        main.bye();
      })
        .add(Ui.img("cross").style("vertical-align:bottom"))
    ];

    const menu = $("table").klass("main").add($("tr")
      .add($("td")
        .adds(lopts))
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
