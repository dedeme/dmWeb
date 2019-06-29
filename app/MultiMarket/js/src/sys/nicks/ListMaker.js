// Copyright 15-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Nicks from "../Nicks.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {_} from "../../I18n.js";
import Wnick from "./Wnick.js";

const $ = Ui.$;

/** Nick page -> List. */
export default class ListMaker {

  /**
   * @param {!Nicks} nicks
   * @param {boolean} withVolume
   */
  constructor (nicks, withVolume) {
    this._nicks = nicks;
    this._withVolume = withVolume;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._newNick = Ui.field("newBt").style("width:100px");
    this._table = $("table").att("align", "center");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  show () {
    this._nicks.entryDiv.removeAll()
      .add($("table").klass("home")
        .add($("tr").add($("td").add(this._newNick)))
        .add($("tr").add($("td").style("text-align:center")
          .add($("button").att("id", "newBt")
            .text(_("New"))
            .on("click", () => {
              this._nicks.newNick(this._newNick.value().trim());
            })))));
    this._nicks.title.text("");
    this._nicks.view.removeAll()
      .add($("hr"))
      .add(this._table);
    this.update();
  }

  /** @private */
  update () {
    const nicks = this._nicks;

    if (nicks.nickList.length === 0) {
      this._table.klass("frame")
        .add($("tr").add($("td").text(_("Without Nicks"))));
    } else {
      const cols = 6;
      const rows = Math.ceil(this._nicks.nickList.length / cols);
      const list = [];

      for (let c = 0; c < cols; ++c) {
        for (let r = 0; r < rows; ++r) {
          list[c + r * cols] = this._nicks.nickList[r + c * rows];
        }
      }

      this._table.klass("frame")
        .style("border-collapse : collapse;");
      let n = 0;
      let tr = $("tr");
      list.forEach(nk => {
        if (n > 0 && n % cols === 0) {
          this._table.add(tr);
          tr = $("tr");
        }
        tr.add($("td")
          .style("width:100px;text-align:center;border-right: solid 1px;")
          .add(nk === undefined
            ? $("span")
            : new Wnick(
              this._nicks,
              nk,
              this._withVolume ? this._nicks.volume[nk.name] || 0 : -1
            ).wg)
        );
        ++n;
      });
      this._table.add(tr);
    }

    this._newNick.e.focus();
  }

}
