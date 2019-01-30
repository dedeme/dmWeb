// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>


import Main from "../Main.js";
import {_} from "../I18n.js";
// eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import Co from "../data/Co.js";

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

    /** @private */
    this._dataSpan = $("span").html("");
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
      entry(_("Summary"), Main.summaryPageId),
      separator(),
      entry(_("Portfolio"), Main.portfolioPageId),
      separator(),
      entry(_("All CO's"), Main.allCosPageId),
      separator(),
      entry(_("Selection"), Main.selectionPageId),
    ];

    const ropts = [
      this._dataSpan,
      entry(_("Log"), Main.logPageId),
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

    this.update();
  }

  update () {
    const lang = this._main.model["lang"];
    const data = this._main.data;
    const mcos = data.cos;
    const cos = [...mcos.keys()].map(k => mcos.get(k));
    const [risk, profits] = Co.allRiskProfits(cos);
    const server = data.server;
    let state = data.state;

    function formatN (n) {
      if (lang === "es") {
        return new Dec(n, 2).toEu();
      }
      return new Dec(n, 2).toEn();
    }
    state = state === "active" ? _("Active")
      : state === "toActive" ? _("To Active")
        : state === "sleeping" ? _("Sleeping")
          : _("To Sleeping")
    ;
    this._dataSpan.html(
      "| " + formatN(risk) + " · " +
      formatN(profits) + " · <font color='00aa41'>" +
      formatN(profits - risk) + "</font> | " +
      server + " | " + state + " | "
    );
  }
}
