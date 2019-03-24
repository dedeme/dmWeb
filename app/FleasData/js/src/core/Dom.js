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

    const lopts = [];
    main.fmodels.forEach((m, i) => {
      if (i === 0) {
        lopts.push(entry(m, m));
      } else {
        lopts.push(separator());
        lopts.push(entry(m, m));
      }
    });

    const ropts = [
      entry(_("Bests"), Main.bestsPageId),
      separator(),
      entry(_("Charts"), Main.chartsPageId),
      $("span").html(" | "),
      entry(_("Settings"), Main.settingsPageId),
      separator(),
      Ui.link(() => {
        main.bye();
      })
        .add(Ui.img("cross").style("vertical-align:bottom"))
    ];

    const menu = $("table").klass("main").add($("tr")
      .add($("td").style("padding-right: 5px")
        .adds(lopts))
      .add($("td").style(
        "text-align:right;white-space: nowrap;" +
        "border-left: 1px solid #000000;" +
        "padding-left: 5px;vertical-align:top;"
      )
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
