// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;

/** Settings page. */
export default class Champions {

  /**
   * @param {!FleasMain} fleasMain Main
   */
  constructor (fleasMain) {
    this._fleasMain = fleasMain;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {void}
   */
  show () {
    this._fleasMain.view.removeAll().add(
      $("div").style("text-align:center").html("Champions")
    );

//    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {Promise}
   */
/*  async update () {
    const rq = {
      "module": "sys",
      "source": "Settings",
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
*/  }

