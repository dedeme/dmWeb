// Copyright 15-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Nicks from "../Nicks.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {_} from "../../I18n.js";

const $ = Ui.$;

/** Nick page -> List. */
export default class ListMaker {

  /** @param {!Nicks} nicks */
  constructor (nicks) {
    this._nicks = nicks;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._table = $("table").att("align", "center");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  show () {
    this._nicks.view.removeAll().add(this._table);
    this.update();
  }

  /** @private */
  update () {
    const nicks = this._nicks;

    if (nicks.nickList.length === 0) {
      this._table.klass("frame")
        .add($("tr").add($("td").text(_("Without Nicks"))));
    } else {
      const list = this._nicks.nickList;

      this._table.klass("frame")
        .style("border-collapse : collapse;");
      let n = 0;
      let tr = $("tr");
      list.forEach(nk => {
        if (n > 0 && n % 10 === 0) {
          this._table.add(tr);
          tr = $("tr");
        }
        tr.add($("td")
          .style("width:65px;text-align:center;border-right: solid 1px;")
          .add(Ui.link((() => { this._nicks.edit(nk.id) }).bind(this._nicks))
            .klass("link").text(nk.name)));
        ++n;
      });
      if (n % 10 !== 0) {
        while (n % 10 !== 0) {
          tr.add($("td"));
          ++n;
        }
        this._table.add(tr);
      }
    }
  }

}
