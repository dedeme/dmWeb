// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Chpass from "./Chpass.js";

const $ = Ui.$;

/** Settings page. */
export default class Settings {

  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    this._main = main;

    // MODEL -------
    // TTTTTTTTTTTTT

    this._lang = "en";

    // VIEW --------
    // TTTTTTTTTTTTT

    this._langDiv = $("div").html("&nbsp;");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {void}
   */
  show () {
    this._main.view.removeAll().add(
      $("div").style("text-align:center")
        .add($("div").klass("head").html(_("Settings")))
        .add($("table").att("align", "center").add($("tr").add($("td")
          .klass("frame")
          .add(this._langDiv)
          .add($("p").html("<p></p>"))
          .add($("div")
            .add(Ui.link(this.goChangePass.bind(this))
              .klass("link").html(_("Change Password")))))))
    );

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {Promise}
   */
  async update () {
    const rq = {
      "rq": "getLang"
    };
    const rp = await Main.client.send(rq);

    this._lang = rp["lang"];

    this._langDiv.removeAll().add(
      Ui.link(this.setLang.bind(this)).klass("link").html(_args(
        _("Change Language to %0"),
        this._lang === "es" ? "EN" : "ES"
      ))
    );
  }

  /**
   * @private
   * @return {Promise}
   */
  async setLang () {
    const rq = {
      "rq": "setLang",
      "lang": this._lang === "en" ? "es" : "en"
    };
    await Main.client.send(rq);
    this._main.update();
  }

  /**
   * @private
   * Show change password page.
   * @return {void}
   */
  goChangePass () {
    new Chpass(this._main).show();
  }
}
