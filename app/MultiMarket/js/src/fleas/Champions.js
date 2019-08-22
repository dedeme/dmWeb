// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Lmenu from "./wgs/Lmenu.js";
import Dtable from "./wgs/Dtable.js";

const $ = e => Ui.$(e);

/**
 * @param {number} nps
 * @return {string}
 */
function nparamsToStr (nps) {
  return "[ " + String(nps) + " ]";
}

/**
 * @param {string} s
 * @return {number}
 */
function nparamsFromStr (s) {
  return Number(s.replace("[", "").replace("]", "").trim());
}

/** Settings page. */
export default class Champions {

  /**
   * @param {!FleasMain} fleasMain Main
   * @param {!Object<string, !Array<?>>} pnames
   *        object "Flea models" -> [[params name][params format]]
   */
  constructor (fleasMain, pnames) {
    this._fleasMain = fleasMain;
    this._pnames = pnames;

    const nparamss = [];
    Object.values(pnames).forEach(v => {
      if (!nparamss.includes(v[0].length)) nparamss.push(v[0].length);
    });
    nparamss.sort();
    this._nparamss = nparamss;

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
     * @param {string} nparams
     * @return {Promise<?>}
     */
    async function selectParams (nparams) {
      const nps = nparamsFromStr(nparams);

      const rq = {
        "module": "fleas",
        "source": "champions",
        "rq": "setNParams",
        "nparams": nps
      };
      await Main.client.rq(rq);

      const rq2 = {
        "module": "fleas",
        "source": "champions",
        "rq": "data",
        "nparams": nps
      };
      const rp = await Main.client.rq(rq2);
      const table = rp["table"];

      if (table.length === "") {
        self._right.removeAll().add(
          $("table").att("align", "center").klass("frame").add($("tr")
            .add($("td").text(_args(_(
              "There are no champions with '%0' parameters"), String(nps)
            ))))
        );
      } else {
        const ps0 = [...It.range(nps)].map(n => "P. " + String(n + 1));
        const ps1 = self._pnames;
        self._right.removeAll().add(
          new Dtable(true, true, [ps0, ps1], table).wg()
        );
      }
    }

    if (this._nparamss.length === 0) {
      this._fleasMain.view.removeAll().add(
        $("table").att("align", "center").klass("frame").add($("tr")
          .add($("td").text(_("Without champions"))))
      );
      return;
    }

    const rq = {
      "module": "fleas",
      "source": "champions",
      "rq": "getNParams"
    };
    const rp = await Main.client.rq(rq);
    let /** number */ nparams = rp["nparams"] || this._nparamss[0];

    if (!this._nparamss.includes(nparams)) {
      nparams = this._nparamss[0];
    }

    this._left.removeAll().add(
      new Lmenu(
        this._nparamss.map(nps => nparamsToStr(nps)),
        selectParams.bind(this),
        nparamsToStr(nparams)
      ).wg
    );

    await selectParams(nparamsToStr(nparams));
  }
}

