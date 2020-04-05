// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";

const $ = e => Ui.$(e);

/**
    Summary chart.
**/
export default class SummaryChart {
  /**
      @param {Array<string>} dates
      @param {Array<number>} values
  **/
  constructor (dates, values) {
    this._dates = dates;
    this._values = values;

    this._wg = $("canvas").klass("frame");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------
  view () {
    const dates = this._dates;
    const values = this._values;
    const dataSize = values.length;

    const backFrame = "#d9d9d9";

    const marginFrame = 5;
    const widthNumbers = 60;
    const heightNumbers = 25;

    const numberFont = "12px sans";
    const numberBaseLine = 5;
    const numberHeight = 12;

    const widthFrame = dataSize + 5 + 2 * marginFrame + widthNumbers;
    const heightFrame = 200 + 2 * marginFrame + heightNumbers;

    const x0Gr = marginFrame + widthNumbers + 0.5;
    const xnGr = widthFrame - marginFrame + 0.5;
    const widthGr = xnGr - x0Gr;

    const y0Gr = marginFrame + 0.5;
    const ynGr = heightFrame - heightNumbers + marginFrame + 0.5;
    const heightGr = ynGr - y0Gr;

    let max = values[0];
    let min = max;
    values.forEach(d => {
      if (d > max) max = d;
      if (d < min) min = d;
    });

    const mAbs = (Math.abs(max) > Math.abs(min)
      ? Math.abs(max)
      : Math.abs(min))
    ;
    const decs = mAbs >= 1000 ? 0 : mAbs >= 10 ? 2 : 4;

    const gap = max / 100;
    const base = Math.floor((min / gap) - 1) * gap;
    const top = Math.ceil(((max - min) / gap) + 2) * gap;
    const step = top / 4;

    // Rectangles -------------------------------------------------------------

    const cv = this._wg
      .att("width", widthFrame)
      .att("height", heightFrame)
      .klass("GrFrame")
      .style("background:" + backFrame);
    const ctx = cv.e.getContext("2d");

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(x0Gr, y0Gr, widthGr, heightGr);
    ctx.fillStyle = "rgba(0, 0, 0)";

    // Left numbers -----------------------------------------------------------

    ctx.font = numberFont;
    for (let i = 0; i < 5; ++i) {
      const tx = new Dec(base + step * i, decs).toIso();
      const text = ctx.measureText(tx);
      ctx.fillText(
        tx,
        (x0Gr - text.width - 4),
        numberBaseLine + ynGr - Math.round(heightGr / 4) * i
      );

      ctx.setLineDash([4, 2]);
      if (i > 0 && i < 4) {
        ctx.beginPath();
        ctx.moveTo(x0Gr, ynGr - Math.round(heightGr / 4) * i);
        ctx.lineTo(xnGr, ynGr - Math.round(heightGr / 4) * i);
        ctx.stroke();
        ctx.closePath();
      }
      ctx.setLineDash([]);
    }

    // Closes -----------------------------------------------------------------

    const xstep = 1;
    let x = x0Gr;
    ctx.beginPath();
    ctx.moveTo(x, ynGr - (values[0] - base) * heightGr / top);
    values.forEach(d => {
      const v = d;
      const y = ynGr - (v - base) * heightGr / top;
      ctx.lineTo(x, y);
      x += xstep;
    });
    ctx.stroke();
    ctx.closePath();

    // Dates ------------------------------------------------------------------

    x = x0Gr - 0.5;
    let dPr = null;
    ctx.setLineDash([4, 2]);
    dates.forEach(d => {
      if (dPr === null) {
        dPr = d;
        return;
      }

      if (d.substring(0, 6) !== dPr.substring(0, 6)) {
        dPr = d;
        if (Number(d.substring(0, 6)) % 2 === 1) {
          const tx = d.substring(4, 6);
          if (tx === "01") ctx.setLineDash([]);
          else ctx.setLineDash([4, 2]);

          ctx.beginPath();
          ctx.moveTo(x - 0.5, y0Gr);
          ctx.lineTo(x - 0.5, ynGr);
          ctx.stroke();
          ctx.closePath();

          const text = ctx.measureText(tx);
          ctx.fillText(
            tx,
            (x - text.width / 2),
            (ynGr + numberHeight)
          );
        }
      }

      x += xstep;
    });
    ctx.setLineDash([]);

    // Last rectangle ---------------------------------------------------------

    ctx.lineWidth = 1;
    ctx.strokeRect(x0Gr, y0Gr, widthGr, heightGr);
  }
}
