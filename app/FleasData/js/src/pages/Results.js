// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import Ui from "../dmjs/Ui.js";
import Lmenu from "../widgets/Lmenu.js";
import Dtable from "../widgets/Dtable.js";
import {_} from "../I18n.js";

const $ = Ui.$;

/** Results page. */
export default class Results {
  /**
   * @param {!Main} main Main
   * @param {string} fmodel Flea model
   */
  constructor (main, fmodel) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /**
     * @private
     * @type{string}
     */
    this._fmodel = fmodel;
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const body = $("div");

    const main = this._main;
    const client = main.client;
    const fmodel = this._fmodel;

    /**
     * @param {string} date
     * @return {Promise<?>}
     */
    async function selectDate (date) {
      const rq = {
        "source": "results",
        "rq": "data",
        "fmodel": fmodel,
        "date": date
      };
      const rp = await client.send(rq);
      const params = rp["params"];
      const table = rp["table"];

      body.removeAll().add(new Dtable(false, params, table).wg());
    }

    const rq = {
      "source": "results",
      "rq": "getDates",
      "fmodel": fmodel
    };
    const rp = await client.send(rq);
    const dates = rp["dates"];
    dates.sort((d1, d2) => Number(d2) - Number(d1));

    if (dates.length === 0) {
      this._main.dom.show(Main.bestsPageId, $("table").att("align", "center")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data"))))
      );
      return;
    }

    main.dom.show(fmodel, $("div")
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top;")
            .add(new Lmenu(dates, selectDate, dates[0]).wg()))
          .add($("td").style("vertical-align:top;")
            .add(body))))
      .add(Ui.upTop("up"))
    );

    selectDate(dates[0]);
  }
}

