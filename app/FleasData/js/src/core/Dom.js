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
   * @param {string} family Fleas family
   * @param {!Domo} o Page to show inside Dom.
   * @return {void}
   */
  show (page, family, o) {
    const main = this._main;
    if (family === "") {
      family = main.families[0];
    }

    const familiesDiv = $("div");

    function entry (id, target) {
      return Ui.link(() => {
        main.go(target, family);
      })
        .klass(target === page ? "frame" : "link").html(id);
    }

    function sourceEntry (fm) {
      return Ui.link(() => {
        main.go(Main.dataPageId, fm);
      })
        .klass(fm === family ? "frame" : "link").html(fm);
    }

    function separator () {
      return $("span").html(" · ");
    }

    function activateData () {
      if (page === Main.dataPageId) {
        familiesDiv.add($("table").att("align", "center")
          .add($("tr")
            .adds(main.families.map(fm => $("td").add(sourceEntry(fm))))));
      }
    }

    const lopts = [
      entry(_("Data"), Main.dataPageId),
    ];

    const ropts = [
      entry(_("Settings"), Main.settingsPageId),
      separator(),
      Ui.link(() => {
        main.bye();
      })
        .add(Ui.img("cross").style("vertical-align:bottom"))
    ];

    const menu = $("table").klass("main").add($("tr")
      .add($("td").style("text-align:right;width:22px;white-space: nowrap;")
        .adds(lopts))
      .add($("td").style("width:100%").add(familiesDiv))
      .add($("td").style("text-align:right;width:22px;white-space: nowrap;")
        .adds(ropts))
    );

    this.showRoot(
      $("div")
        .add(menu)
        .add($("hr"))
        .add(o)
    );

    activateData();
  }
}
