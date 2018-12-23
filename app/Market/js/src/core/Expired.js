// Copyright 12-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;

/** Expired page. */
export default class Expired {
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

  /** @return {void} */
  show () {
    const td = "padding:0px 10px 0px 10px;";
    const link = "<a href=''>" + _("here") + "</a>";
    const w = $("div")
      .add($("p").att("style", "text-align:center")
        .html(`<big><b>${Main.app}</b></big>`))
      .add($("table").att("class", "main")
        .add($("tr")
          .add($("td")
            .add($("table")
              .att("class", "border")
              .att("width", "100%")
              .att("style",
                "background-color: #f8f8f8;" +
                "border-collapse: collapse;")
              .add($("tr")
                .add($("td")
                  .att("style", td)
                  .html("<p>" + _("Session is expired.") + "<p>" +
                    "<p><b>" +
                    _args(_("Click %0 to continue."),
                      link) + "</b></p>")))))));
    this._main.dom.showRoot(w);
  }
}
