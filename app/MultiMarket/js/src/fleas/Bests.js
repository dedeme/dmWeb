// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Lmenu from "./wgs/Lmenu.js";
import Dtable from "./wgs/Dtable.js";

const $ = Ui.$;

/** Settings page. */
export default class Bests {

  /**
   * @param {!FleasMain} fleasMain Main
   * @param {!Array<string>} models Flea models
   */
  constructor (fleasMain, models) {
    this._fleasMain = fleasMain;
    this._models = models;

    // VIEW ------------------
    // TTTTTTTTTTTTTTTTTTTTTTT

    this._left = $("div");
    this._right = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {void}
   */
  show () {
    this._fleasMain.view.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top;")
            .add(this._left))
          .add($("td").style("vertical-align:top;")
            .add(this._right))))
      .add(Ui.upTop("up"))
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {Promise}
   */
  async update () {
    const self = this;

    /**
     * @param {string} model
     * @return {Promise<?>}
     */
    async function selectModel (model) {
      const rq = {
        "module": "fleas",
        "source": "bests",
        "rq": "setModel",
        "model": model
      };
      await Main.client.rq(rq);

      const rq2 = {
        "module": "fleas",
        "source": "bests",
        "rq": "data",
        "model": model
      };
      const rp = await Main.client.rq(rq2);
      const params = rp["params"];
      const table = rp["table"];

      if (params === "") {
        self._right.removeAll().add(
          $("table").att("align", "center").klass("frame").add($("tr")
            .add($("td").text(_args(_("Model '%0' not found"), model))))
        );
      } else {
        self._right.removeAll().add(new Dtable(true, params, table).wg());
      }
    }

    if (this._models.length === 0) {
      this._fleasMain.view.removeAll().add(
        $("table").att("align", "center").klass("frame").add($("tr")
          .add($("td").text(_("Without models"))))
      );
      return;
    }

    const rq = {
      "module": "fleas",
      "source": "bests",
      "rq": "getModel"
    };
    const rp = await Main.client.rq(rq);
    let /** string */ model = rp["model"] || this._models[0];
    if (!this._models.includes(model)) {
      model = this._models[0];
    }

    this._left.removeAll().add(
      new Lmenu(this._models, selectModel.bind(this), model).wg
    );

    await selectModel(model);
  }

}

