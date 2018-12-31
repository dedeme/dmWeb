// Copyright 27-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import DateDm from "../dmjs/DateDm.js";

const $ = Ui.$;

const mkCanvas = data => {
  const cv = $("canvas").att("width", 600).att("height", 200)
    .klass("frame");
  const ctx = cv.e.getContext("2d");

  if (data.length <= 1) {
    const tx = _("Without data");
    ctx.font = "32px sans";
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (600 - text.width) / 2, 100);
    return cv;
  }

  let max = data[0][1];
  let min = max;
  data.forEach(dv => {
    const v = dv[1];
    if (v > max) max = v;
    if (v < min) min = v;
  });
  const base = Math.floor(min / 1000) * 1000;
  const top = Math.floor((max - min + 1000) / 1000) * 1000;
  const step = top / 4;

  ctx.lineWidth = 1;
  ctx.strokeRect(100.5, 10.5, 490, 160);

  ctx.font = "12px sans";
  for (let i = 0; i < 5; ++i) {
    const tx = new Dec(base + step * i, 0).toEu();
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (95 - text.width), 175 - 40 * i);

    ctx.setLineDash([4, 2]);
    if (i > 0 && i < 4) {
      ctx.beginPath();
      ctx.moveTo(100.5, 170.5 - 40 * i);
      ctx.lineTo(589.5, 170.5 - 40 * i);
      ctx.stroke();
      ctx.closePath();
    }
    ctx.setLineDash([]);
  }

  const xstep = Math.floor(490 / (data.length - 1));
  let x = 100.5;
  ctx.beginPath();
  ctx.moveTo(x, 170.5 - (data[0][1] - base) * 160 / top);
  data.forEach(dv => {
    const v = dv[1];
    const y = 170.5 - (v - base) * 160 / top;
    ctx.lineTo(x, y);
    x += xstep;
  });
  ctx.stroke();
  ctx.closePath();

  return cv;
};

const lastGr = data => {
  const cv = mkCanvas(data);
  return $("div").add(cv);
};

const allGr = data => {
  const cv = mkCanvas(data);
  return $("div").add(cv);
};

/** Update page. */
export default class Graphics {
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
   * @return {Promise}
   */
  async show () {
    const rq = {
      "source": "graphics",
      "rq": "idata"
    };
    const rp = await this._main.client.send(rq);
    const data = rp["data"];
    data.reverse();
    const date = DateDm.now().add(-365).toBase();
    const valueDiv = () => {
      const tb = $("table").att("align", "center").klass("frame");
      if (data.length === 0) {
        tb.add($("tr").add($("td").html(_("Without data"))));
        return tb;
      }
      const d = data[data.length - 1];
      tb.add($("tr")
        .add($("td").html(
          "<big>" +
          DateDm.fromStr(d[0]).toString() +
          " : " +
          (this._main.model["lang"] === "es"
            ? new Dec(d[1], 2).toEu()
            : new Dec(d[1], 2).toEn()) +
          "</big>"
        )));
      if (data.length === 1 || data[data.length - 2][1] !== d[1]) {
        return tb;
      }
      tb.add($("tr")
        .add($("td").add(Ui.link(async () => {
          const rq = {
            "source": "graphics",
            "rq": "removeDuplicate"
          };
          await this._main.client.send(rq);
          this.show();
        }).klass("link").html(_("Remove duplicate")))));
      return tb;
    };
    this._main.dom.show(Main.graphicsPageId,
      $("div").style("text-align:center")
        .add(valueDiv())
        .add($("div").klass("head").html(_("Last")))
        .add(lastGr(data.filter(dv => dv[0] >= date)))
        .add($("div").klass("head").html(_("All")))
        .add(allGr(data))
    );
  }

}
