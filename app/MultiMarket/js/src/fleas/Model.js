// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Lmenu from "./wgs/Lmenu.js";
import Dtable from "./wgs/Dtable.js";

const $ = Ui.$;

/** Model page. */
export default class Model {

  /**
   * @param {!FleasMain} fleasMain Main
   * @param {string} model Model to show
   */
  constructor (fleasMain, model) {
    this._fleasMain = fleasMain;
    this._model = model;

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
     * @param {string} date
     * @return {Promise<?>}
     */
    async function selectDate (date) {
      const rq = {
        "module": "fleas",
        "source": "model",
        "rq": "data",
        "model": self._model,
        "date": date
      };
      const rp = await Main.client.rq(rq);
      const params = rp["params"];
      const table = rp["table"];

      if (params === "") {
        self._right.removeAll().add(
          $("table").att("align", "center").klass("frame").add($("tr")
            .add($("td").text(_args(_(
              "Model '%0-%1' not found"), self._model, date
            ))))
        );
      } else {
        self._right.removeAll().add(
          new Dtable(false, false, params, table).wg()
        );
      }
    }

    const rq = {
      "module": "fleas",
      "source": "model",
      "rq": "getDates",
      "model": this._model
    };
    const rp = await Main.client.rq(rq);
    const dates = rp["dates"];
    dates.sort((d1, d2) => Number(d2) - Number(d1));

    if (dates.length === 0) {
      this._fleasMain.view.removeAll()
        .add($("table").att("align", "center")
          .add($("tr").add($("td").klass("frame").html(
            _("Without data")))))
      ;
      return;
    }

    this._left.removeAll().add(
      new Lmenu(dates, selectDate.bind(this), dates[0]).wg
    );

    selectDate(dates[0]);
  }
}
