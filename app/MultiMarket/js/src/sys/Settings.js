// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import SysMain from "./SysMain.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Chpass from "./Chpass.js";

const $ = Ui.$;

/** Settings page. */
export default class Settings {
  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {
    /**
     * @private
     * @type {!SysMain}
     */
    this._sysMain = sysMain;
  }

  /**
   * @param {string} lang It can be "en" or "es"
   * @return {Promise}
   */
  async setLang (lang) {
    const sysMain = this._sysMain;
    const rq = {
      "module": "sys",
      "page": "Settings",
      "rq": "setLang",
      "lang": lang
    };
    await sysMain.client.send(rq);
    sysMain.main.run();
  }

  /**
   * Show change password page.
   * @return {void}
   */
  goChangePass () {
    new Chpass(this._sysMain.main).show();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /**
   * @return {Promise}
   */
  async show () {
    const sysMain = this._sysMain;
    const rq = {
      "module": "sys",
      "page": "Settings",
      "rq": "getLang"
    };
    const rp = await sysMain.client.send(rq);

    const lang = rp["lang"];

    const langDiv = $("div")
      .add(Ui.link(() => {
        this.setLang(lang === "es" ? "en" : "es");
      })
        .klass("link").html(_args(
          _("Change Language to %0"),
          lang === "es" ? "EN" : "ES"
        )));
    const passDiv = $("div")
      .add(Ui.link(() => {
        this.goChangePass();
      })
        .klass("link").html(_("Change Password")));

    const opts = [
      langDiv,
      $("p").html("<p></p>"),
      passDiv
    ];

    sysMain.dom.show("settings", $("div").style("text-align:center")
      .add($("div").klass("head").html(_("Settings")))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .klass("frame")
        .adds(opts))))
    );
  }
}

