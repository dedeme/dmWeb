// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Server from "../data/Server.js"; //eslint-disable-line
import Wserver from "./servers/Wserver.js";
import Names from "./servers/Names.js";
import Codes from "./servers/Codes.js";
import Download from "./servers/Download.js";
import Menu from "../wgs/Menu.js";
import Nick from "../data/Nick.js";

const $ = Ui.$;

const NAMES = "names";
const DAILY = "daily";
const HISTORIC = "historic";
const CODES = "codes";

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

    /** @type {!Array<!Server>} */
    this._serverList = [];

    /** @type {number} */
    this._serverSelId = -1;

    /** @type {string} */
    this._serverTab = "";

    // VIEW --------
    // TTTTTTTTTTTTT

    this._newServer = Ui.field("newBt").style("width:100px");
    this._serverListWg = $("div");
    this._view0 = $("div");
    this._view = $("div");
    this._viewMenu = new Menu();
    this._viewMenuName = $("span");
  }

  /** @return {!SysMain} */
  get sysMain () {
    return this._sysMain;
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }

  /** @return {number} */
  get serverSelId () {
    return this._serverSelId;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {!Domo}
   */
  get listWg () {
    return $("table").klass("submenu")
      .adds(this._serverList.map(sv =>
        $("tr").add($("td").add(
          new Wserver(this, sv).wg
        ))
      ));
  }

  /**
   * @private
   * @return {!Domo}
   */
  get serverWg () {
    this._viewMenu = new Menu();
    const menu = this._viewMenu;
    menu.addLeft(Menu.mkOption(
      NAMES, _("Names"), () => { this.setServerTab(NAMES) }
    ));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkOption(
      DAILY, _("Daily"), () => { this.setServerTab(DAILY) }
    ));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkOption(
      HISTORIC, _("Historic"), () => { this.setServerTab(HISTORIC) }
    ));
    menu.addLeft(Menu.separator2());
    menu.addLeft(Menu.mkOption(
      CODES, _("Codes"), () => { this.setServerTab(CODES) }
    ));
    menu.addRight(this._viewMenuName);
    return $("table").klass("main")
      .add($("tr").add($("td").add(menu.wg)))
      .add($("tr").add($("td").add(this._view)))
    ;
  }

  /**
   * @private
   * @return {!Domo}
   */
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
        .add($("td").style("vertical-align:top").add(this._view0)))
      .add(Ui.upTop("up"))
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
    const rq = {
      "module": "sys",
      "source": "Servers",
      "rq": "idata"
    };
    const rp = await Main.client.send(rq);

    this._serverList = rp["serverList"].map(e => Server.fromJs(e));

    if (this._serverList.length === 0) {
      this._serverListWg.removeAll().add($("div").klass("frame")
        .style("text-align:center")
        .html(_("Without<br>Servers")));
      this._view0.removeAll().add($("table")
        .klass("frame").att("align", "center")
        .add($("tr").add($("td").text(_("Without Data")))));
    } else {
      this._serverSelId = rp["serverSelId"];
      this._serverTab = rp["serverTab"];
      if (this._serverTab === "") {
        this._serverTab = NAMES;
      }
      this._serverList.sort((sv1, sv2) =>
        sv1.shortName > sv2.shortName ? 1 : -1
      );
      this._serverListWg.removeAll().add(this.listWg);

      if (this._serverSelId === -1) {
        this._view0.removeAll().add($("table")
          .klass("frame").att("align", "center")
          .add($("tr").add($("td").text(_("No Server Selected")))));

      } else {
        this._view0.removeAll().add(this.serverWg);
        this.viewUpdate();
      }
    }
  }

  /**
   * @private
   * @return {!Promise}
   */
  async viewUpdate () {
    const sv = this._serverList.find(sv => sv.id === this._serverSelId);
    if (sv === undefined) {
      throw (new Error(`Server with id '${this._serverSelId}' is unknown`));
    }

    this._viewMenuName.removeAll().text(sv.name);
    this._viewMenu.setSelected(this._serverTab);

    this._view.removeAll();
    switch (this._serverTab) {
    case NAMES:
      this._view.add(new Names(this, sv).wg);
      break;
    case DAILY:
      this._view.add(new Download(false, this, sv).wg);
      break;
    case HISTORIC:
      this._view.add(new Download(true, this, sv).wg);
      break;
    case CODES: {
      const rq = {
        "module": "sys",
        "source": "Servers",
        "rq": "nicks"
      };
      const rp = await Main.client.send(rq);
      const nicks = rp["nicks"].map(js => Nick.fromJs(js));
      this._view.add(new Codes(this, sv, nicks).wg);
      break;
    }
    default:
      throw (new Error(`viewId ${this._serverTab} is unknown`));
    }
  }

  /**
   * @private
   * @return {!Promise}
   */
  async newServer () {
    const shortName = this._newServer.value().trim();
    if (shortName === "") {
      alert(_("Server name is missing"));
      return;
    }

    const rq = {
      "module": "sys",
      "source": "Servers",
      "rq": "new",
      "shortName": shortName
    };
    const rp = await Main.client.send(rq);

    const ok = rp["ok"];
    if (!ok) {
      alert(_args(_("Short name '%0' is duplicated"), shortName));
      return;
    }

    this.update();
  }

  /**
   * @param {Server} server
   * @return {!Promise}
   */
  async del (server) {
    if (!confirm(_args(_("Delete '%0'?"), server.shortName))) {
      return;
    }
    const rq = {
      "module": "sys",
      "source": "Servers",
      "rq": "del",
      "id": server.id
    };
    await Main.client.send(rq);
    this.update();
  }

  /**
   * @param {Server} server
   * @return {!Promise}
   */
  async edit (server) {
    const rq = {
      "module": "sys",
      "source": "Servers",
      "rq": "setServerSelId",
      "id": server.id
    };
    await Main.client.send(rq);
    this.update();
  }

  /**
   * @private
   * @param {string} id
   * @return {!Promise}
   */
  async setServerTab (id) {
    const rq = {
      "module": "sys",
      "source": "Servers",
      "rq": "setServerTab",
      "id": id
    };
    await Main.client.send(rq);
    this.update();
  }

}

