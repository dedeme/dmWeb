// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Nick page. */

import Main from "../Main.js";
import Menu from "../wgs/Menu.js";
import Msg from "../wgs/Msg.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import {MSG} from "../consts.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Nick from "../data/Nick.js";
import Editor from "./nicks/Editor.js";
import ListMaker from "./nicks/ListMaker.js";

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

function msgs (msg) {
  switch (msg) {
  case "": return _("Download or test successfully done");
  case "quotes": return _("Some quotes were corrected");
  case "nick": return _("Nick not found");
  case "server": return _("Wrong server data");
  case "net": return _("Connection fail");
  case "server/quotes": return _("Wrong server data and quotes corrected");
  case "net/quotes": return _("Connection fail and quotes corrected");
  }
  throw new Error(`Unknown message '${msg}'`);
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

    this._entryDiv = $("div");
    this._menu = new Menu(false);
    this._title = $("div").style("font-size: 24px;");
    this._view = $("div");

    this._msg = new Msg();
  }

  /** @return {!Domo} */
  get entryDiv () {
    return this._entryDiv;
  }

  /** @return {!Domo} */
  get title () {
    return this._title;
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

  /** @private */
  setWait (nickName) {
    this._msg.setWg($("div")
      .add($("div").style("text-align:center")
        .add(Ui.img("wait2.gif").klass("frame")))
      .add($("div").style("text-align:center").html(nickName))
    );
  }

  /** @private */
  get wg () {
    return $("div")
      .add(this._msg.wg)
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("vertical-align:top;width:5px")
            .add(this._entryDiv))
          .add($("td").style("vertical-align:top")
            .add(this._menu.wg)
            .add($("table").klass("main").add($("tr")
              .add($("td").style("text-align:left").add(this._title))
              .add($("td").style("text-align:right")
                .add(Ui.link(this.download.bind(this))
                  .klass("link").text(_("Download")))
                .add($("span").text(" · "))
                .add(Ui.link(this.test.bind(this))
                  .klass("link").text(_("Test")))
              )))))
        .add($("tr")
          .add($("td").att("colspan", 2)
            .add(this._view))))
      .add(Ui.upTop("up"))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._sysMain.view.removeAll().add(this.wg);
    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @param {number} id Selected Id or -1 if no one is selected.
   * @return {!Promise}
   */
  async setNickSelId (id) {
    this._nickSelId = id;
    const rq = {
      "module": "sys",
      "source": "Nicks",
      "rq": "setNickSelId",
      "id": id
    };
    await Main.client.send(rq);
  }

  /** @return {!Promise} */
  async list () {
    this._menu.setSelected("list");
    await this.setNickSelId(-1);
    new ListMaker(this).show();
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
      Menu.mkOption("list", _("List"), this.list.bind(this))
    );

    if (this._nickList.length === 0) {
      this._menu.setSelected("list");
      this._title.text("");
      this.view.removeAll().add($("table").klass("frame").att("align", "center")
        .add($("tr").add($("td").text(_("Without Nicks")))));
    } else {
      this._model = rp["model"];
      this._nickSelId = rp["nickSelId"];
      this._nickList.sort((nk1, nk2) => nk1.name > nk2.name ? 1 : -1);
      if (this._nickSelId < 0) {
        this.list();
      } else {
        this.edit(this._nickSelId);
      }
    }
  }

  /**
   * @param {string} nick
   * @return {!Promise}
   */
  async newNick (nick) {
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

    this.update();
  }

  /**
   * @param {string} nick
   * @param {string} oldNick
   * @return {!Promise}
   */
  async modifyNick (nick, oldNick) {
    if (nick === "") {
      alert(_("Nick name is missing"));
      return;
    }
    if (nick === oldNick) {
      alert(_("Nick name was not changed"));
      return;
    }

    const rq = {
      "module": "sys",
      "source": "Nicks",
      "rq": "modNick",
      "nickId": this._nickSelId,
      "nickName": nick
    };
    const rp = await Main.client.send(rq);

    const ok = rp["ok"];
    if (!ok) {
      alert(_args(_("Nick '%0' is duplicated"), nick));
      return;
    }

    this.update();
  }

  /** @return {!Promise} */
  async download () {
    const nickList = this._nickSelId === -1
      ? this._nickList.map(n => n.id)
      : [this._nickSelId]
    ;

    const len = nickList.length;
    let err = MSG.OK;
    let msg = "";
    this._msg.show(true);
    for (const nickId of nickList) {
      const nick = this._nickList.find(n => n.id === nickId);
      if (nick === undefined) {
        continue;
      }
      this.setWait(nick.name);
      const rq = {
        "module": "sys",
        "source": "Nicks",
        "rq": "download",
        "nickId": nickId
      };
      const rp = await Main.client.sendLongRun(rq);
      const e = rp["err"];
      err = err === MSG.OK ||
        (err === MSG.WARNING && e !== MSG.OK) ||
        e === MSG.ERROR ? e : err
      ;
      if (len === 1) {
        msg = rp["msg"];
      }
    }
    this._msg.show(false);

    if (len === 1) {
      this._msg.showMsg(err, msgs(msg));
    } else {
      msg = err === MSG.OK
        ? _("Download successfully done")
        : err === MSG.WARNING
          ? _("Some quotes were corrected")
          : _("Download has errors")
      ;
      this._msg.showMsg(err, msg);
    }

    this.update();
  }

  /** @return {!Promise} */
  async test () {
    const nickList = this._nickSelId === -1
      ? this._nickList.map(n => n.id)
      : [this._nickSelId]
    ;

    const len = nickList.length;
    let err = MSG.OK;
    let msg = "";
    this._msg.show(true);
    for (const nickId of nickList) {
      const nick = this._nickList.find(n => n.id === nickId);
      if (nick === undefined) {
        continue;
      }
      this.setWait(nick.name);
      const rq = {
        "module": "sys",
        "source": "Nicks",
        "rq": "test",
        "nickId": nickId
      };
      const rp = await Main.client.send(rq);
      const e = rp["err"];
      err = err === MSG.OK ||
        (err === MSG.WARNING && e !== MSG.OK) ||
        e === MSG.ERROR ? e : err
      ;

      if (len === 1) {
        msg = rp["msg"];
      }
    }
    this._msg.show(false);

    if (len === 1) {
      this._msg.showMsg(err, msgs(msg));
    } else {
      msg = err === MSG.OK
        ? _("Test successfully done")
        : err === MSG.WARNING
          ? _("Some quotes should be corrected")
          : _("Test has errors")
      ;
      this._msg.showMsg(err, msg);
    }
  }

}

