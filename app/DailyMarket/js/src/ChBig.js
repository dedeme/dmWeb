// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
// eslint-disable-next-line
import Domo from "./dmjs/Domo.js";
import Dec from "./dmjs/Dec.js";
import Ui from "./dmjs/Ui.js";
// eslint-disable-next-line
import Co from "./data/Co.js";

const $ = Ui.$;

/** Small chart. */
export default class ChBig {
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
    this._cv = $("canvas").att("width", 600).att("height", 320).klass("frame");

    /** @private */
    this._table = $("table").klass("main")
      .add($("tr")
        .add($("td").att("colspan", 2)
          .add(this._cv)))
    ;
  }

  /** @return {!Domo} */
  wg () {
    return this._table;
  }

  /**
   * @param {!Array<!Array<number>>} hps
   */
  update (hps) {
    const self = this;
    function formatN (n, d) {
      if (self._main.model["lang"] === "es") {
        return new Dec(n, d).toEu();
      }
      return new Dec(n, d).toEn();
    }

    const ctx = this._cv.e.getContext("2d");

    if (hps.length <= 1) {
      this._cv.style("background-color:#f9f9f9");

      ctx.fillStyle = "rgba(249, 249, 249)";
      ctx.fillRect(0.5, 0.5, 598, 318);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(90.5, 10.5, 504, 280);
      ctx.fillStyle = "rgba(0, 0, 0)";

      ctx.lineWidth = 1;
      ctx.strokeRect(10.5, 10.5, 504, 280);
      return;
    }

    let max = hps[0][1];
    let min = max;
    hps.forEach(hv => {
      const v = hv[1];
      if (v > max) max = v;
      if (v < min) min = v;
    });
    const gap = max / 1000;
    const base = Math.floor((min / gap) - 1) * gap;
    const top = Math.ceil(((max - min) / gap) + 2) * gap;
    const step = top / 4;

    ctx.fillStyle = "#f9f9f9";
    ctx.fillRect(0.5, 0.5, 598, 318);
    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(90.5, 10.5, 504, 280);
    ctx.fillStyle = "rgba(0, 0, 0)";

    ctx.font = "14px sans";
    for (let i = 0; i < 5; ++i) {
      const tx = formatN(base + step * i, 2);
      const text = ctx.measureText(tx);
      ctx.fillText(tx, (80 - text.width), 297 - 70 * i);

      ctx.setLineDash([4, 2]);
      if (i > 0 && i < 4) {
        ctx.beginPath();
        ctx.moveTo(90.5, 290.5 - 70 * i);
        ctx.lineTo(592.5, 290.5 - 70 * i);
        ctx.stroke();
        ctx.closePath();
      }
      ctx.setLineDash([]);
    }

    const xstep = 500 / (hps.length - 1);
    const x = 92.5;
    ctx.beginPath();
    ctx.moveTo(x, 290.5 - (hps[0][1] - base) * 280 / top);
    hps.forEach((hs, i) => {
      const v = hs[1];
      const y = 290.5 - (v - base) * 280 / top;
      const x = 92.5 + xstep * i;
      ctx.lineTo(x, y);
    });
    ctx.stroke();
    ctx.closePath();

    ctx.setLineDash([4, 2]);
    let hour = hps[0][0];
    hps.forEach((hs, i) => {
      const h = hs[0];
      if (h !== hour) {
        hour = h;
        ctx.beginPath();
        const x = 92.5 + xstep * i;
        ctx.moveTo(x, 290.5);
        ctx.lineTo(x, 10.5);
        ctx.stroke();
        ctx.closePath();

        const text = ctx.measureText(String(h));
        ctx.fillText(String(h), x - text.width / 2, 310.5);
      }
    });
    ctx.setLineDash([]);

    ctx.lineWidth = 1;
    ctx.strokeRect(90.5, 10.5, 504, 280);

  }

}
