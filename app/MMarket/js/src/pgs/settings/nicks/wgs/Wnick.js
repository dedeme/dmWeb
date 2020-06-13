// Copyright 07-Apr-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {_} from "../../../../I18n.js";
import Domo from "../../../../dmjs/Domo.js";  //eslint-disable-line
import Ui from "../../../../dmjs/Ui.js";
import Dec from "../../../../dmjs/Dec.js";
import Nick from "../../../../data/Nick.js"; //eslint-disable-line
import Nicks from "../Nicks.js"; //eslint-disable-line

const $ = e => Ui.$(e);
const img = (id, title) => Ui.img(id).att("title", title);
const limg = (id, title) => Ui.lightImg(id).att("title", title);
const emptyBt = (title) => $("div")
  .style(`padding:5px;
          border: 1px solid #002040;border-radius: 6px;
          background: #d0ddde;`)
  .att("title", title)
;

/**
    Wnick widget.
**/
export default class Wnick {

  /**
      @param {!Domo} wg
      @param {!Nicks} nicksPg
      @param {!Nick} nick
      @param {boolean} isModel
      @param {number} volume
  **/
  constructor (wg, nicksPg, nick, isModel, volume) {
    this._wg = wg;
    this._nicksPg = nicksPg;
    this._nick = nick;
    this._isModel = isModel;
    this._volume = volume;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    const nick = this._nick;

    const model = this._isModel
      ? img("star", _("Model"))
      : nick.isSel
        ? Ui.link(() => { this._nicksPg.setModel(nick) })
          .add(limg("star2", _("Model")))
        : Ui.link(() => { this._nicksPg.del(nick) })
          .add(img("delete", _("Delete")))
    ;

    const isSel = nick.isSel
      ? Ui.link(() => { this._nicksPg.setIsSel(nick) })
        .add(img(
          this._volume < 1000000
            ? "flag2"
            : "flag1",
          _("Selection")
        ))
      : Ui.link(() => { this._nicksPg.setIsSel(nick) })
        .add(emptyBt(_("Selection")))
    ;

    const nk = Ui.link(() => {
      this._nicksPg.selectNick(nick);
    }).klass("link").text(nick.name);
    if (this._volume > 0) {
      nk.att("title", new Dec(this._volume, 0).toIso());
    }

    this._wg
      .removeAll()
      .add($("table")
        .add($("tr")
          .add($("td").add(model))
          .add($("td").add(isSel))
          .add($("td").add(nk))))
    ;
  }
}
