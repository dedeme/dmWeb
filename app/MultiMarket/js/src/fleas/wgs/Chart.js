// Copyright 28-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
import {_} from "../../I18n.js";
const $ = Ui.$;

/** Company char. */
export default class Chart {


  /**
   * @param {boolean} isBig
   * @param {!Array<!Array<?>>} qs
   * @return {!Domo}
   */
  static mk (isBig, qs) {
    // Without data -----------------------------------------------------------
    if (qs.length <= 1) {
      let widthFrame = 260;
      let heightFrame = 150;
      let widthGr = 250;
      let heightGr = 140;
      if (isBig) {
        widthFrame = 610;
        heightFrame = 315;
        widthGr = 600;
        heightGr = 305;
      }

      const cv = $("canvas")
        .att("width", widthFrame)
        .att("height", heightFrame)
        .klass("GrFrame")
        .style("background:#d9d9d9");
      const ctx = cv.e.getContext("2d");

      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(5, 5, widthGr, heightGr);
      ctx.fillStyle = "rgba(0, 0, 0)";

      const tx = _("Without data");
      ctx.font = "32px sans";
      const text = ctx.measureText(tx);
      ctx.fillText(tx, (widthGr - text.width) / 2, heightGr / 2);
      return cv;
    }

    // Values -----------------------------------------------------------------

    if (!isBig) {
      qs = qs.slice(-250);
    }

    const qsSize = qs.length;

    const lastQ = qs[qs.length - 1];
    const lastQ1 = qs[qs.length - 2];
    const backFrame = (lastQ[2] > lastQ[1] && lastQ1[2] < lastQ1[1])
      ? "#f0f0ff"
      : (lastQ[2] < lastQ[1] && lastQ1[2] > lastQ1[1])
        ? "#fff0f0"
        : "#d9d9d9"
    ;

    const marginFrame = 5;
    const widthNumbers = isBig ? 60 : 40;
    const heightNumbers = isBig ? 25 : 20;

    const numberFont = isBig ? "12px sans" : "10px sans";
    const numberBaseLine = isBig ? 5 : 4;
    const numberHeight = isBig ? 12 : 10;

    const widthFrame = qsSize + 5 + 2 * marginFrame + widthNumbers;
    const heightFrame = (isBig ? 315 : 140) + 2 * marginFrame + heightNumbers;

    const x0Gr = marginFrame + widthNumbers + 0.5;
    const xnGr = widthFrame - marginFrame + 0.5;
    const widthGr = xnGr - x0Gr;

    const y0Gr = marginFrame + 0.5;
    const ynGr = heightFrame - heightNumbers + marginFrame + 0.5;
    const heightGr = ynGr - y0Gr;

    let max = qs[0][1];
    let min = max;
    qs.forEach(e => {
      let v = e[1];
      if (v > max) max = v;
      if (v < min) min = v;
      v = e[2];
      if (v > max) max = v;
      if (v < min) min = v;
    });
    const gap = max / 100;
    const base = Math.floor((min / gap) - 1) * gap;
    const top = Math.ceil(((max - min) / gap) + 2) * gap;
    const step = top / 4;

    // Rectangles -------------------------------------------------------------

    const cv = $("canvas")
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
      const tx = new Dec(base + step * i, 2).toEu();
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
    ctx.moveTo(x, ynGr - (qs[0][1] - base) * heightGr / top);
    qs.forEach(cs => {
      const v = cs[1];
      const y = ynGr - (v - base) * heightGr / top;
      ctx.lineTo(x, y);
      x += xstep;
    });
    ctx.stroke();
    ctx.closePath();

    // Refs -------------------------------------------------------------------

    x = x0Gr - 0.5;
    qs.forEach(e => {
      const v = e[2];
      const y = ynGr - (v - base) * heightGr / top;
      if (v > e[1]) {
        ctx.fillStyle = "rgba(0, 129, 255)";
      } else if (e[2] < e[1]) {
        ctx.fillStyle = "rgba(255, 40, 0)";
      }

      ctx.fillRect(x, y, 1, 1);
      ctx.fillStyle = "rgba(0, 0, 0)";
      x += xstep;
    });

    // Dates ------------------------------------------------------------------

    x = x0Gr - 0.5;
    let dPr = null;
    ctx.setLineDash([4, 2]);
    qs.forEach(e => {
      const d = e[0];
      if (dPr === null) {
        dPr = d;
        return;
      }

      if (d.substring(0, 6) !== dPr.substring(0, 6)) {
        dPr = d;
        if (Number(d.substring(0, 6)) % 2 === 1) {
          ctx.beginPath();
          ctx.moveTo(x - 0.5, y0Gr);
          ctx.lineTo(x - 0.5, ynGr);
          ctx.stroke();
          ctx.closePath();

          const tx = d.substring(4, 6);
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

    return cv;
  }
}
