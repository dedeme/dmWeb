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
   * @param {!Domo} o Page to show inside Dom.
   * @return {void}
   */
  show (o) {
    const main = this._main;
    const model = main.model;

    function entry (id) {
      return Ui.link(() => {
        main.go(id);
      }).klass(id === model.sel ? "frame" : "link")
        .text(id)
      ;
    }

    function separator () {
      return $("span").html(" · ");
    }

    const paths = model.paths.filter(p => p.show && p.ok);

    const menu = $("table").klass("main").add($("tr")
      .add($("td")
        .add((model.sel === "@"
          ? $("a").klass("frame")
            .att("href", "?@")
          : $("a").att("href", "?@"))
          .add(Ui.img("asterisk").att("align", "top")))
        .adds(paths.reduce((r, p) => {
          r.push(separator());
          r.push(entry(p.id));
          return r;
        }, [])))
      .add($("td").style("text-align:right")
        .add(Ui.link(() => {
          main.bye();
        })
          .add(Ui.img("cross").style("vertical-align:bottom"))
        )
      )
    );

    this.showRoot(
      $("div")
        .add(menu)
        .add($("hr"))
        .add(o)
    );
  }
}
