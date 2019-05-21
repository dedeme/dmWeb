// Copyright 15-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Nicks from "../Nicks.js";
import Ui from "../../dmjs/Ui.js";

const $ = Ui.$;

/** Nick page -> List. */
export default class Editor {

  /**
   * @param {!Nicks} nicks
   * @param {number} nickId Nick to edit
   */
  constructor (nicks, nickId) {
    this._nicks = nicks;
    this._nickId = nickId;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  show () {
    this._nicks.view.removeAll().add($("p").html("unimplemented"));
  }


}
