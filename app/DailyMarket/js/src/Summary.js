// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import ChBig from "./ChBig.js";

const $ = Ui.$;

/** Update page. */
export default class Summary {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /** @private */
    this._ch = new ChBig(main);
  }

  showData () {
    const mcos = this._main.data.cos;
    const cos = [...mcos.keys()]
      .map(k => mcos.get(k))
      .filter(co => co.stocks > 0);

    const hps = [];
    [...It.range(cos[0].qs.length)].forEach(i => {
      let sum = -10000;
      cos.forEach(e => {
        if (i < e.qs.length) {
          sum += e.stocks * (e.qs[i][1] - e.price);
        }
      });
      hps.push([cos[0].qs[i][0], sum]);
    });
    this._ch.update(hps);
  }

  chartsTable () {
    return $("table").klass("frame").att("align", "center")
      .add($("tr")
        .add($("td")
          .add(this._ch.wg())));
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show(Main.summaryPageId, $("div").style("text-align:center;")
      .add($("div").klass("head").style("padding-bottom:4px")
        .html(_("Summary")))
      .add(this.chartsTable())
    );
    this.showData();
  }
}

