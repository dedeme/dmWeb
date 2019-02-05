// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Dec from "./dmjs/Dec.js";
import Clock from "./dmjs/Clock.js";
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
    this._perc = $("div");

    /** @private */
    this._ch = new ChBig(main);

    /** @private */
    this._charTable = $("table").klass("frame").att("align", "center")
      .add($("tr")
        .add($("td")
          .add(this._ch.wg)));

  }

  showData () {
    const self = this;
    function formatN (n, d) {
      if (self._main.model["lang"] === "es") {
        return new Dec(n, d).toEu();
      }
      return new Dec(n, d).toEn();
    }

    const mcos = this._main.data.cos;
    const cos = [...mcos.keys()]
      .map(k => mcos.get(k))
      .filter(co => co.stocks > 0);

    const hps = [];
    let start = 0;
    let end = 1;
    [...It.range(cos[0].qs.length)].forEach(i => {
      let sum = -10000;
      cos.forEach(e => {
        if (i < e.qs.length) {
          sum += e.stocks * (e.qs[i][1] - e.price);
        }
      });
      hps.push([cos[0].qs[i][0], sum]);
      if (i === 0) {
        start = sum;
      }
      end = sum;
    });

    const profits = end - start;
    const ratio = profits / start;
    this._perc.removeAll()
      .add($("span").klass("frame").style(
        "font-size:x-large;color:" +
        (ratio > 0 ? "#00AAFF" : ratio < 0 ? "#FF8100" : "#000000")
      ).html(" " + formatN(ratio * 100, 2) + " % | " +
        formatN(profits, 2) + " "))
    ;

    this._ch.update(hps);
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show(Main.summaryPageId, $("div").style("text-align:center;")
      .add($("div").klass("head").style("padding-bottom:8px")
        .html(_("Summary")))
      .add(this._perc)
      .add(this._charTable)
      .add(new Clock().mkWg().klass("frame")
        .style(
          "background:radial-gradient(#000333,#e6f6f6);" +
          "margin-top: 8px;"
        ))
    );
    this.showData();
  }
}

