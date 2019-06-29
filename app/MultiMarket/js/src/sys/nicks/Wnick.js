// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../../Main.js";
import Nicks from "../Nicks.js"; //eslint-disable-line
import {_args, _} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
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
   * @param {!Nicks} nicks
   * @param {!Nick} nick
   * @param {number} volume
   */
  constructor (nicks, nick, volume) {
    this._nicks = nicks;
    this._nick = nick;
    this._volume = volume;
    this._isModel = nicks.model === nick.id;
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

    const nk = Ui.link(() => {
      this._nicks.edit(nick.id);
    }).klass("link").text(nick.name);
    if (this._volume > 0) {
      nk.att("title", new Dec(this._volume, 0).toEu());
    }

    return $("table")
      .add($("tr")
        .add($("td").add(model))
        .add($("td").add(isSel))
        .add($("td").add(nk)))
    ;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  async setModel () {
    const rq = {
      "module": "sys",
      "source": "nicks/Wnick",
      "rq": "setModel",
      "id": this.id
    };
    await Main.client.send(rq);
    this._nicks.update();
  }

  /** @private */
  async del () {
    if (!confirm(_args(_("Delete '%0'?"), this._nick.name))) {
      return;
    }
    const rq = {
      "module": "sys",
      "source": "nicks/Wnick",
      "rq": "del",
      "id": this.id
    };
    await Main.client.send(rq);
    this._nicks.update();
  }

  /** @private */
  async setIsSel () {
    const data = {
      "module": "sys",
      "source": "nicks/Wnick",
      "rq": "setIsSel",
      "id": this.id,
      "value": !this._nick.isSel
    };
    await Main.client.send(data);
    this._nicks.update();
  }
}
