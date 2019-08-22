// Copyright 01-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import ModalBox from "../dmjs/ModalBox.js";
import {_} from "../I18n.js";
import {MSG} from "../consts.js";

const $ = e => Ui.$(e);

/** MsgModal widget */
export default class Msg {

  constructor () {
    this._boxDiv = Ui.$("div");
    this._box = new ModalBox(this._boxDiv);
  }

  /** @return {!Domo} */
  get wg () {
    return this._box.wg;
  }

  /**
   * @param {!Domo} o
   * @return {void}
   */
  setWg (o) {
    this._boxDiv.removeAll().add(o);
  }

  /**
   * @param {boolean} value
   * @return {void}
   */
  show (value) {
    this._box.show(value);
  }

  /**
   * @param {number} type It can be MSG_OK, MSG.WARNING or MSG.ERROR
   * @param {string} msg Html message
   * @return {void}
   */
  showMsg (type, msg) {
    this.setWg($("table")
      .add($("tr")
        .add($("td").style("valign:middle;width:50px").att("rowspan", 3)
          .add(Ui.img(
            type === MSG.OK ? "well2" : type === MSG.WARNING ? "info" : "error2"
          )))
        .add($("td").style("text-align:left").html(msg)))
      .add($("tr").add($("td").add($("hr"))))
      .add($("tr")
        .add($("td").style("text-align: right")
          .add($("button").text(_("Close"))
            .on("click", () => this.show(false)))))
    );
    this.show(true);
  }

}
