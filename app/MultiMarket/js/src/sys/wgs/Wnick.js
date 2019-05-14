// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../../Main.js";
import SysMain from "../SysMain.js";
import {_args, _} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Nick from "../../data/Nick.js"; //eslint-disable-line

// VIEW ------------

const $ = Ui.$;
const img = (id, title) => Ui.img(id).att("title", title);
const limg = (id, title) => Ui.lightImg(id).att("title", title);
const emptyBt = (title) => $("div")
  .style("padding:5px;" +
         "border: 1px solid #002040;border-radius: 6px;" +
         "background: #d0ddde;")
  .att("title", title)
;

/** Wnick widget. */
export default class Wnick {

  /**
   * @param {!SysMain} sysMain
   * @param {?number} model
   * @param {!Nick} nick
   */
  constructor (sysMain, model, nick) {
    this._sysMain = sysMain;
    this._nick = nick;
    this._isModel = model !== null && model === nick.id;
  }

  /** @return {number} The nick identifier (a number)*/
  get id () {
    return this._nick.id;
  }


  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    const nick = this._nick;

    const model = this._isModel
      ? img("star", _("Model"))
      : nick.isSel
        ? Ui.link(this.setModel.bind(this)).add(limg("star2", _("Model")))
        : Ui.link(this.del.bind(this)).add(img("delete", _("Delete")))
    ;

    const isSel = nick.isSel
      ? Ui.link(this.setIsSel.bind(this)).add(img("flag1", _("Selection")))
      : Ui.link(this.setIsSel.bind(this)).add(emptyBt(_("Selection")))
    ;

    return $("table")
      .add($("tr")
        .add($("td").add(model))
        .add($("td").add(isSel))
        .add($("td").add(Ui.link(() => {
          this.edit(nick.id);
        }).klass("link").text(nick.name))))
    ;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  async setModel () {
    const rq = {
      "module": "sys",
      "source": "Wnick",
      "rq": "setModel",
      "id": this.id
    };
    await Main.client.send(rq);
    this._sysMain.run();
  }

  /** @private */
  async del () {
    if (!confirm(_args(_("Delete '%0'?"), this._nick.name))) {
      return;
    }
    const rq = {
      "module": "sys",
      "source": "Wnick",
      "rq": "del",
      "id": this.id
    };
    await Main.client.send(rq);
    this._sysMain.run();
  }

  /** @private */
  async setIsSel () {
    const data = {
      "module": "sys",
      "source": "Wnick",
      "rq": "setIsSel",
      "id": this.id,
      "value": !this._nick.isSel
    };
    const rq = await Main.client.send(data);
    if (rq["ok"]) {
      this._sysMain.run();
    } else {
      alert(_args(_("'%0' has server data missing"), this._nick.name));
    }
  }

  /** @private */
  async edit (id) {
    const data = {
      "module": "sys",
      "source": "Wnick",
      "rq": "edit",
      "id": id,
      "menu": SysMain.editPageId
    };
    await Main.client.send(data);
    this._sysMain.run();
  }

}
