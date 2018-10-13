// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Chpass from "./Chpass.js";

const $ = Ui.$;

/** Settings page. */
export default class Settings {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * @param {string} lang It can be "en" or "es"
   * @return {Promise}
   */
  async setLang (lang) {
    const main = this._main;
    const rq = {
      "page": "settings",
      "rq": "setLang",
      "lang": lang
    };
    await main.client.send(rq);
    main.run();
  }

  /**
   * Show change password page.
   * @return {void}
   */
  goChangePass () {
    new Chpass(this._main).show();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const main = self._main;
    const lang = main.conf["lang"];

    const langDiv = $("div")
      .add(Ui.link(() => {
        self.setLang(lang === "es" ? "en" : "es");
      })
        .klass("link").html(_args(
          _("Change Language to %0"),
          lang === "es" ? "EN" : "ES"
        )));
    const passDiv = $("div")
      .add(Ui.link(() => {
        self.goChangePass();
      })
        .klass("link").html(_("Change Password")));

    const opts = [
      langDiv,
      $("p").html("<p></p>"),
      passDiv
    ];

    main.dom.show("settings", "", $("div").style("text-align:center")
      .add($("h2").html(_("Settings")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .adds(opts))))
    );
  }
}

