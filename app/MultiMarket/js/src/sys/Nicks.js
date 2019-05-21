// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import Menu from "../wgs/Menu.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Nick from "../data/Nick.js";
import Wnick from "./nicks/Wnick.js";
import Editor from "./nicks/Editor.js";
import ListMaker from "./nicks/ListMaker.js";
import Downloader from "./nicks/Downloader.js";
import Tester from "./nicks/Tester.js";

const $ = Ui.$;

/**
 * @param {!Array<!Nick>} list
 * @return {string}
 */
function mkStatistics (list) {
  const total = list.length;
  let sel = 0;
  list.forEach(nk => { if (nk.isSel) ++sel; });
  return _args(_("Total: %0. Selected: %1."), String(total), String(sel));
}

/** Nick page. */
export default class Nicks {
  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {

    this._sysMain = sysMain;

    /** @type {!Array<!Nick>} */
    this._nickList = [];

    /** @type {number} */
    this._nickSelId = -1;

    /** @type {number} */
    this._model = -1;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._newNick = Ui.field("newBt").style("width:100px");
    this._nickListWg = $("div");
    this._menu = new Menu();
    this._view = $("div");
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }

  /** @return {!Array<!Nick>} */
  get nickList () {
    return this._nickList;
  }

  /** @return {number} Nick list selected */
  get nickSelId () {
    return this._nickSelId;
  }

  /** @return {number} Nick model*/
  get model () {
    return this._model;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {!Domo}
   */
  mkList () {
    return $("table").klass("submenu")
      .adds(this._nickList.map(nk =>
        $("tr").add($("td").add(new Wnick(this, nk).wg))
      ));
  }

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
        .add($("td").style("vertical-align:top")
          .add(this._menu.wg)
          .add(this._view)))
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

  /**
   * @private
   * @param {number} id Selected Id or -1 if no one is selected.
   * @return {!Promise}
   */
  async setNickSelId (id) {
    const rq = {
      "module": "sys",
      "source": "Nicks",
      "rq": "setNickSelId",
      "id": id
    };
    await Main.client.send(rq);
    this._nickSelId = id;
    this._nickListWg.removeAll().add(this.mkList());
  }

  /** @return {!Promise} */
  async list () {
    this._menu.setSelected("list");
    await this.setNickSelId(-1);
    new ListMaker(this).show();
    this._newNick.e.focus();
  }

  /** @return {void} */
  download () {
    this._menu.setSelected("download");
    if (this._nickList.length === 0) {
      new ListMaker(this).show();
    } else {
      alert("download unimplemented");
    }
  }

  /** @return {void} */
  test () {
    this._menu.setSelected("test");
    if (this._nickList.length === 0) {
      new ListMaker(this).show();
    } else {
      alert("test unimplemented");
    }
  }

  /**
   * @param {number} nickId
   * @return {!Promise}
   */
  async edit (nickId) {
    this._menu.setSelected("");
    await this.setNickSelId(nickId);
    new Editor(this, nickId).show();
  }

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "sys",
      "source": "Nicks",
      "rq": "idata"
    };
    const rp = await Main.client.send(rq);

    this._nickList = rp["nickList"].map(e => Nick.fromJs(e));

    this._menu.reset();

    this._menu.addLeft($("span").text(mkStatistics(this._nickList)));
    this._menu.addRight(
      Menu.mkOption("test", _("Test"), this.test.bind(this))
    );
    this._menu.addRight(Menu.separator());
    this._menu.addRight(
      Menu.mkOption("download", _("Download"), this.download.bind(this))
    );
    this._menu.addRight(Menu.separator());
    this._menu.addRight(
      Menu.mkOption("list", _("List"), this.list.bind(this))
    );

    if (this._nickList.length === 0) {
      this._nickListWg.removeAll().add($("div").klass("main")
        .style("text-align:center")
        .html(_("Without<br>Nicks")));
      this._menu.setSelected("list");
      this.view.removeAll().add($("table").klass("frame").att("align", "center")
        .add($("tr").add($("td").text(_("Without Nicks")))));
    } else {
      this._model = rp["model"];
      this._nickSelId = rp["nickSelId"];
      this._nickList.sort((nk1, nk2) => nk1.name > nk2.name ? 1 : -1);
      this._nickListWg.removeAll().add(this.mkList());
      if (this._nickSelId < 0) {
        this.list();
      } else {
        alert("unimplemented");
      }
    }
  }

  /**
   * @private
   * @return {!Promise}
   */
  async newNick () {
    const nick = this._newNick.value().trim();

    if (nick === "") {
      alert(_("Nick name is missing"));
      return;
    }

    const rq = {
      "module": "sys",
      "source": "Nicks",
      "rq": "newNick",
      "nick": nick
    };
    const rp = await Main.client.send(rq);

    const ok = rp["ok"];
    if (!ok) {
      alert(_args(_("Nick '%0' is duplicated"), nick));
      return;
    }

    this._newNick.value("");
    this.update();
  }

}

