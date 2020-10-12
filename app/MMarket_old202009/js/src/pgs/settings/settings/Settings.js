// Copyright 06-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import {_, _args} from "../../../I18n.js";
import ChangePass from "./ChangePass.js";
import Cts from "../../../data/Cts.js";

const $ = e => Ui.$(e);

/**
    Settings page.
**/
export default class Settings {

  /**
      @param {!Domo} wg
      @param {string} lang
  **/
  constructor (wg, lang) {
    this._wg = wg;
    this._lang = lang;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg
      .removeAll()
      .add($("div")
        .style("text-align:center")
        .add($("div")
          .klass("head")
          .html(_("Settings")))
        .add($("table")
          .att("align", "center")
          .add($("tr")
            .add($("td")
              .klass("frame")
              .add($("div")
                .add(Ui.link(() => { this.changeLang() })
                  .klass("link")
                  .html(_args(
                    _("Change Language to %0"),
                    this._lang === "es" ? "EN" : "ES"
                  ))))
              .add($("p")
                .html("<p></p>"))
              .add($("div")
                .add(Ui.link(() => this.changePass())
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
   * @private
   * @return {!Promise<void>}
   */
  async changeLang () {
    await Cts.client.send({
      "module": "settings",
      "source": "settings",
      "rq": "setLang",
      "lang": this._lang === "en" ? "es" : "en"
    });
    location.reload();
  }

  /**
   * @private
   * Show change password page.
   * @return {void}
   */
  changePass () {
    const wg = this._wg;
    wg.removeAll();
    const pg = new ChangePass(this._wg, () => { this.view() });
    pg.focus();
  }

}

