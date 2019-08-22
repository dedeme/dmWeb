// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Lmenu from "./wgs/Lmenu.js";
import Wcharts from "./wgs/Wcharts.js";

const $ = e => Ui.$(e);

/** Charts page. */
export default class Charts {

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
            .add(this._left)) //new Lmenu(fmodels, selectModel, model).wg()))
          .add($("td").style("vertical-align:top;")
            .add(this._right))))
      .add(Ui.upTop("up"))
    ;

    this.update();
  }

  /**
   * @param {string} model
   * @return {Promise<?>}
   */
  async showModel (model) {
    const rq = {
      "module": "fleas",
      "source": "charts",
      "rq": "nicks",
      "model": model
    };
    const rp = await Main.client.rq(rq);
    const list = rp["list"];

    this._right.removeAll().add(
      new Wcharts(Wcharts.BESTS, model, list).wg
    );
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {Promise}
   */
  async update () {
    const rq = {
      "module": "fleas",
      "source": "charts",
      "rq": "getModel"
    };
    const rp = await Main.client.rq(rq);
    let /** string */ model = rp["model"] || this._models[0];
    if (!this._models.includes(model)) {
      model = this._models[0];
    }

    this._left.removeAll().add(
      new Lmenu(
        this._models,
        model => this.showModel(model),
        model
      ).wg
    );

    await this.showModel(model);
  }

}
