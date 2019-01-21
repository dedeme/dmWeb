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
export default class ChSmall {
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
    this._leftHeader = $("span");

    /** @private */
    this._rightHeader = $("span");

    /** @private */
    this._cv = $("canvas").att("width", 305).att("height", 160).klass("frame");

    /** @private */
    this._table = $("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;")
          .add(this._leftHeader))
        .add($("td").style("text-align:right;")
          .add(this._rightHeader)))
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
   * @param {string} nick
   * @param {Co} co
   */
  update (nick, co) {
    const self = this;
    function formatN (n, d) {
      if (self._main.model["lang"] === "es") {
        return new Dec(n, d).toEu();
      }
      return new Dec(n, d).toEn();
    }

    const ctx = this._cv.e.getContext("2d");

    if (co.qs.length <= 1) {
      this._cv.style("background-color:#f9f9f9");
      this._leftHeader.removeAll()
        .add($("span").html(nick))
        .add($("span").html("&nbsp;&nbsp;"))
        .add(this._main.isSel(nick)
          ? Ui.link(() => { this._main.removeSel(nick) }).add(Ui.img("minus"))
          : Ui.link(() => { this._main.addSel(nick) }).add(Ui.img("plus"))
        )
      ;
      this._rightHeader.html("");


      ctx.fillStyle = "rgba(249, 249, 249)";
      ctx.fillRect(0.5, 0.5, 304, 159);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(45.5, 5.5, 252, 140);
      ctx.fillStyle = "rgba(0, 0, 0)";

      ctx.lineWidth = 1;
      ctx.strokeRect(45.5, 5.5, 252, 140);
      return;
    }

    const lastQ = co.qs[co.qs.length - 1][1];
    const lcolor = co.signal > 0 ? "#00AAFF" : "#FF8100";
    const ratio = co.signal > 0 ? lastQ / co.signal : -co.signal / lastQ;
    const backg =
      ratio >= 1 ? co.signal > 0 ? "#fff0f0" : "#f0f0ff"
        : "#f9f9f9";
    const signalAbs = Math.abs(co.signal);

    let max = co.qs[0][1];
    let min = max;
    co.qs.forEach(hv => {
      const v = hv[1];
      if (v > max) max = v;
      if (v < min) min = v;
    });
    const gap = max / 1000;
    const base = Math.floor((min / gap) - 1) * gap;
    const top = Math.ceil(((max - min) / gap) + 2) * gap;
    const step = top / 4;

    this._cv.style("background-color:" + backg);

    this._leftHeader.removeAll()
      .add($("span").html(nick))
      .add($("span").html("&nbsp;&nbsp;"))
      .add(this._main.isSel(nick)
        ? Ui.link(() => { this._main.removeSel(nick) }).add(Ui.img("minus"))
        : Ui.link(() => { this._main.addSel(nick) }).add(Ui.img("plus"))
      )
      .add($("span").html("&nbsp;&nbsp;"))
      .add(co.dayRatio > 0
        ? Ui.img("flag-blue")
        : co.dayRatio < 0
          ? Ui.img("flag-red")
          : Ui.img("flag-black")
      )
      .add($("span").html("<br>"))
      .add($("span").style("color:" + lcolor)
        .html(formatN(ratio * 100, 2) + "%"))
    ;

    if (co.stocks > 0) {
      this._rightHeader.html(
        formatN(co.risk, 2) +
        "<br>" +
        formatN(co.profits, 2)
      );
    } else {
      this._rightHeader.html("");
    }

    ctx.fillStyle = backg;
    ctx.fillRect(0.5, 0.5, 304, 159);
    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(45.5, 5.5, 252, 140);
    ctx.fillStyle = "rgba(0, 0, 0)";

    ctx.font = "10px sans";
    for (let i = 0; i < 5; ++i) {
      const tx = formatN(base + step * i, 2);
      const text = ctx.measureText(tx);
      ctx.fillText(tx, (40 - text.width), 150 - 35 * i);

      ctx.setLineDash([4, 2]);
      if (i > 0 && i < 4) {
        ctx.beginPath();
        ctx.moveTo(45.5, 145.5 - 35 * i);
        ctx.lineTo(294.5, 145.5 - 35 * i);
        ctx.stroke();
        ctx.closePath();
      }
      ctx.setLineDash([]);
    }

    const xstep = 250 / (co.qs.length - 1);
    const x = 46.5;
    ctx.beginPath();
    ctx.moveTo(x, 145.5 - (co.qs[0][1] - base) * 140 / top);
    co.qs.forEach((hs, i) => {
      const v = hs[1];
      const y = 145.5 - (v - base) * 140 / top;
      const x = 46.5 + xstep * i;
      ctx.lineTo(x, y);
    });
    ctx.stroke();
    ctx.closePath();

    if (signalAbs > min && signalAbs < max) {
      if (co.signal > 0) {
        ctx.strokeStyle = "rgba(0, 170, 255)";
      } else {
        ctx.strokeStyle = "rgba(255, 249, 129)";
      }
      const y = 145.5 - (signalAbs - base) * 140 / top;
      ctx.beginPath();
      ctx.moveTo(46.5, y);
      ctx.lineTo(295.5, y);
      ctx.stroke();
      ctx.closePath();
      ctx.strokeStyle = "rgba(0, 0, 0)";
    }

    ctx.setLineDash([4, 2]);
    let hour = co.qs[0][0];
    co.qs.forEach((hs, i) => {
      const h = hs[0];
      if (h !== hour) {
        hour = h;
        ctx.beginPath();
        const x = 46.5 + xstep * i;
        ctx.moveTo(x, 145.5);
        ctx.lineTo(x, 5.5);
        ctx.stroke();
        ctx.closePath();

        const text = ctx.measureText(String(h));
        ctx.fillText(String(h), x - text.width / 2, 155.5);
      }
    });
    ctx.setLineDash([]);

    ctx.lineWidth = 1;
    ctx.strokeRect(45.5, 5.5, 252, 140);

  }

}
