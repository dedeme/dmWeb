// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import SysMain from "./SysMain.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line

const $ = Ui.$;

const DESCRIPTION = "description";
const HISTORIC = "historic";
const DAILY = "daily";
const COMPANY = "company";
const NICKS = "nicks";

/*
const VIEWS = {
  DESCRIPTION: _("Description"),
  HISTORIC: _("Historic"),
  DAILY: _("Daily"),
  COMPANY: _("Company"),
  NICKS: _("Nicks"),
  };
*/
/** Nicks - Extra pages. */
export default class Servers {
  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {
    /** @type {!SysMain} */
    this._sysMain = sysMain;

    /** @type {!string} */
    this._viewSel = "";

    /** @type {!Array<string>} */
    this._serverList = [];

    /** @type {number} */
    this._serverSelId = -1;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._newServer = Ui.field("newBt").style("width:100px");
    this._serverListWg = $("div");
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
            .add($("tr").add($("td").add(this._newServer)))
            .add($("tr").add($("td").style("text-align:center")
              .add($("button").att("id", "newBt")
                .text(_("New"))
                .on("click", this.newServer.bind(this)))))
            .add($("tr").add($("td").add($("hr"))))
            .add($("tr").add($("td").add(this._serverListWg)))))
        .add($("td").style("vertical-align:top").add(this._view)))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._sysMain.view.removeAll().add(this.wg);
    this.update();
    this._newServer.e.focus();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const data = {
      "module": "sys",
      "source": "Servers",
      "rq": "idata"
    };
    const rp = await Main.client.send(data);

    this._serverList = rp["serverList"];
    if (this._serverList.length === 0) {
      this._serverListWg.add($("div").klass("frame")
        .style("text-align:center")
        .html(_("Without<br>Servers")));
      this.view.add($("table").klass("frame").att("align", "center")
        .add($("tr").add($("td").text(_("Without Data")))));
    } else {
      alert("Not implemented");
    }
  }

  /**
   * @private
   * @return {void}
   */
  newServer () {
  }
}

