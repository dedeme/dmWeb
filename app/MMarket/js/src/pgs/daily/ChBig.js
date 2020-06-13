// Copyright 08-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Dec from "../../dmjs/Dec.js";
import Ui from "../../dmjs/Ui.js";

const $ = e => Ui.$(e);

/** Small chart. */
export default class ChSmall {
  // Static --------------------------------------------------------------------
  /**
      @param {!Array<number>} hours
      @param {!Array<number>} quotes
      @param {number} ratio Ratio to current profits to define background.
      @return !Domo
  **/
  static mk (hours, quotes, ratio) {
    const hs = hours.map(e => e);
    hs.reverse();
    const qs = quotes.map(e => e);
    qs.reverse();
    const cv = $("canvas")
      .att("width", 305 * 2)
      .att("height", 160 * 2)
      .klass("frame")
    ;
    const ctx = cv.e.getContext("2d");

    const grGapX = 1 * 2;
    const grIncrX = 46.5 * 2;
    const grWidth = 250 * 2;
    const grIncrY = 5.5 * 2;
    const grHeight = 140 * 2;

    if (qs.length <= 1) {
      const backg = "#f9f9f9";
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5 * 2, 0.5 * 2, 304 * 2, 159 * 2);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";
    } else {
      const backg = ratio > 0
        ? "#f0f0ff"
        : ratio < 0 ? "#fff0f0" : "#f9f9f9"
      ;
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5 * 2, 0.5 * 2, 304 * 2, 159 * 2);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";

      let max = -1000000;
      let min = 1000000;
      for (const q of qs) {
        if (q > max) max = q;
        if (q < min) min = q;
      }

      const incr = (max - min) * 0.04;
      max += incr;
      min -= incr;
      const dif = max - min;

      const toGrY = n => grIncrY + grHeight - (n - min) * grHeight / dif;
      const toGrX = n => n * grWidth / (qs.length - 1) + grIncrX;

      // Horizontal grid
      ctx.font = "12px sans";
      for (let i = 0; i < 5; ++i) {
        const y = min + dif * i / 4;
        const tx = new Dec(y, 2).toIso();
        const text = ctx.measureText(tx);
        ctx.fillText(
          tx,
          grIncrX - grGapX - text.width - 5,
          toGrY(y) + 3
        );

        ctx.setLineDash([4, 2]);
        if (i > 0 && i < 4) {
          ctx.beginPath();
          ctx.moveTo(grIncrX - grGapX, toGrY(y));
          ctx.lineTo(grWidth + grIncrX + grGapX, toGrY(y));
          ctx.stroke();
          ctx.closePath();
        }
        ctx.setLineDash([]);
      }

      // Vertical grid
      let lastHour = hs[0];
      ctx.setLineDash([4, 2]);
      for (let i = 0; i < hs.length; ++i) {
        if (hs[i] !== lastHour) {
          lastHour = hs[i];

          const tx = new Dec(lastHour, 0).toIso();
          const text = ctx.measureText(tx);
          ctx.fillText(
            tx,
            toGrX(i) - text.width / 2,
            grIncrY + grHeight + 12
          );

          ctx.beginPath();
          ctx.moveTo(toGrX(i), grIncrY);
          ctx.lineTo(toGrX(i), grIncrY + grHeight);
          ctx.stroke();
          ctx.closePath();
        }
      }
      ctx.setLineDash([]);

      // Quotes
      for (let i = 0; i < qs.length - 1; ++i) {
        ctx.beginPath();
        ctx.moveTo(toGrX(i), toGrY(qs[i]));
        ctx.lineTo(toGrX(i + 1), toGrY(qs[i + 1]));
        ctx.stroke();
        ctx.closePath();
      }
    }


    ctx.lineWidth = 1;
    ctx.strokeRect(90.5, 10.5, grWidth + grGapX * 2, grHeight);

    return $("table").klass("main")
      .add($("tr")
        .add($("td")
          .add(cv)))
    ;
  }


}
