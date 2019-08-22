// Copyright 08-Jun-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import AccMain from "../AccMain.js";
import Main from "../../Main.js";
//eslint-disable-next-line
import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";

const $ = e => Ui.$(e);

/** Downloader widget. */
export default class Downloader {
  /**
   * @param {!AccMain} accMain
   */
  constructor (accMain) {
    this._accMain = accMain;

    this._wg = $("span")
      .add(Ui.link(() => { this.download() })
        .add(Ui.img("download2").style("vertical-align:middle")));
  }

  /**
   * @return {!Domo}
   */
  get wg () {
    return this._wg;
  }

  /**
   * @private
   */
  async download () {
    this._wg.removeAll()
      .add(Ui.img("wait.gif").style("vertical-align:middle"));

    const rq = {
      "module": "acc",
      "source": "Downloader",
      "rq": "download",
    };

    await Main.client.rq(rq);

    window.location.assign("");
  }

}
