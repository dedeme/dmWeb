// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import SysMain from "./SysMain.js";
import {_, _args} from "../I18n.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Nick from "../data/Nick.js";
import Wnick from "./wgs/Wnick.js";

const $ = Ui.$;

/** Nicks - Extra pages. */
export default class Nicks {
  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {

    this._sysMain = sysMain;

    /** @type {!Array<string>} */
    this._nickList = [];

    /** @type {number} */
    this._nickSelId = -1;

    /** @type {number} */
    this._total = 0;

    /** @type {number} */
    this._sel = 0;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._newNick = Ui.field("newBt").style("width:100px");
    this._nickListWg = $("div");
    this._view = $("div");
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  get wg () {
    return $("table").klass("main")
      .add($("tr")
        .add($("td").style("vertical-align:top;width:5px")
          .add($("table").klass("home")
            .add($("tr").add($("td").add(this._newNick)))
            .add($("tr").add($("td").style("text-align:center")
              .add($("button").att("id", "newBt")
                .text(_("New"))
                .on("click", this.newNick.bind(this)))))
            .add($("tr").add($("td").add($("hr"))))
            .add($("tr").add($("td").add(this._nickListWg)))))
        .add($("td").style("vertical-align:top").add(this._view)))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._sysMain.view.removeAll().add(this.wg);
    this.update();
    this._newNick.e.focus();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const data = {
      "module": "sys",
      "source": "Nicks",
      "rq": "idata"
    };
    const rp = await Main.client.send(data);

    this._nickList = rp["nickList"];
    if (this._nickList.length === 0) {
      this._nickListWg.add($("div").klass("frame")
        .style("text-align:center")
        .html(_("Without<br>Nicks")));
      this.view.add($("table").klass("frame").att("align", "center")
        .add($("tr").add($("td").text(_("Without Data")))));
    } else {
      this._nickSelId = rp["nickSelId"];
      alert("Not implemented");
    }
  }

  /**
   * @private
   * @return {void}
   */
  newNick () {
  }

}

