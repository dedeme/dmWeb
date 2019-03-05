// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import Ui from "../dmjs/Ui.js";
import Lmenu from "../widgets/Lmenu.js";
import Dtable from "../widgets/Dtable.js";

const $ = Ui.$;

/** Bests page. */
export default class Bests {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const body = $("div");
    const client = this._main.client;

    /**
     * @param {string} model
     * @return {Promise<?>}
     */
    async function selectModel (model) {
      const rq = {
        "source": "bests",
        "rq": "setModel",
        "model": model
      };
      await client.send(rq);

      const rq2 = {
        "source": "bests",
        "rq": "data",
        "model": model
      };
      const rp = await client.send(rq2);
      const params = rp["params"];
      const table = rp["table"];

      body.removeAll().add(new Dtable(true, params, table).wg());
    }

    const main = this._main;
    const fmodels = main.fmodels;

    const rq = {
      "source": "bests",
      "rq": "getModel"
    };
    const rp = await client.send(rq);
    let model = rp["model"];
    if (model === "") {
      model = fmodels[0];
    }

    main.dom.show(Main.bestsPageId, $("div")
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top;")
            .add(new Lmenu(fmodels, selectModel, model).wg()))
          .add($("td").style("vertical-align:top;")
            .add(body))))
      .add(Ui.upTop("up"))
    );

    selectModel(model);
  }
}

