// Copyright 11-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Profits charts
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import It from "../../../dmjs/It.js";
import Ui from "../../../dmjs/Ui.js";
import Dec from "../../../dmjs/Dec.js";
import DateDm from "../../../dmjs/DateDm.js";
import {_} from "../../../I18n.js";

const $ = e => Ui.$(e);

const ALL = 0;
const YEAR = 1;
const MONTH = 2;

const mkCanvas = (data, type) => {
  let backg = "#e9e9e9";
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

  if (data[0][1][0] < data[data.length - 1][1][0]) {
    backg = "#e9e9f2";
  } else if (data[0][1][0] > data[data.length - 1][1][0]) {
    backg = "#f2e9e9";
  }
  cv.style("background:" + backg);

  let max = data[0][1][0];
  let min = max;
  data.forEach(dvs => {
    It.range(3).each(i => {
      const v = dvs[1][i];
      if (v > max) max = v;
      if (v < min) min = v;
    });
  });
  const base = Math.floor((min / 1000) - 1) * 1000;
  const top = Math.ceil(((max - min) / 1000) + 2) * 1000;
  const step = top / 4;

  ctx.fillStyle = "rgba(255, 255, 255)";
  ctx.fillRect(100.5, 10.5, 490, 160);
  ctx.fillStyle = "rgba(0, 0, 0)";

  ctx.lineWidth = 1;
  ctx.strokeRect(100.5, 10.5, 490, 160);

  ctx.font = "12px sans";
  for (let i = 0; i < 5; ++i) {
    const tx = new Dec(base + step * i, 0).toIso();
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

  const xstep = 488 / (data.length - 1);
  It.range(3).each(i => {
    ctx.strokeStyle = i === 0 ? "rgba(0, 129, 255)"
      : i === 1 ? "rgba(0, 0, 0)"
        : "rgba(255, 40, 0)";
    let x = 100.5;
    ctx.beginPath();
    ctx.moveTo(x, 170.5 - (data[0][1][i] - base) * 160 / top);
    data.forEach(dvs => {
      const v = dvs[1][i];
      const y = 170.5 - (v - base) * 160 / top;
      ctx.lineTo(Math.floor(x) + 0.5, y);
      x += xstep;
    });
    ctx.stroke();
    ctx.closePath();
  });
  ctx.strokeStyle = "rgba(0, 0, 0)";

  let x = 100.5;
  let lastD = type === ALL ? data[0][0].substring(2, 4)
    : data[0][0].substring(4, 6);
  ctx.setLineDash([4, 2]);
  data.forEach((dv, ix) => {
    let d = type === ALL ? dv[0].substring(2, 4) : dv[0].substring(4, 6);
    if (
      (type !== MONTH && lastD !== d) ||
      (type === MONTH && ix !== 0 && ix % 5 === 0)
    ) {
      if (type === MONTH) {
        d = dv[0].substring(6, 8);
      } else {
        lastD = d;
      }
      const x2 = Math.floor(x) + 0.5;
      const text = ctx.measureText(d);
      ctx.fillText(d, x2 - text.width / 2, 180.5);

      ctx.beginPath();
      ctx.moveTo(x2, 10.5);
      ctx.lineTo(x2, 170.5);
      ctx.stroke();
      ctx.closePath();
    }
    x += xstep;
  });
  ctx.setLineDash([]);

  return cv;
};

const lastMonthGr = data => {
  const cv = mkCanvas(data, MONTH);
  return $("div").add(cv);
};

const lastYearGr = data => {
  const cv = mkCanvas(data, YEAR);
  return $("div").add(cv);
};

const allGr = data => {
  const cv = mkCanvas(data, ALL);
  return $("div").add(cv);
};

/**
    Profits charts
**/
export default class ProfitsWg {
  /**
      @param {!Domo} wg
      @param {!Array<?>} data Has the following structure:
        [
          date:string [total:number, acc:number, risk:number],
          date [total, acc, risk],
          ... (from after to before)
          until N dates
        ]
      @return void
  **/
  static mk (wg, data) {
    if (data.length <= 1) {
      wg
        .removeAll()
        .add($("table")
          .att("align", "center")
          .klass("frame")
          .add($("tr")
            .add($("td")
              .html(_("Without data")))))
      ;
      return;
    }
    const d = data[0];
    data = data.map(e => e);
    data.reverse();
    const aYearAgo = DateDm.now().add(-365).toBase();
    const currentYear = DateDm.now().format("%Y0101");
    wg
      .removeAll()
      .style("text-align:center")
      .add($("table")
        .att("align", "center")
        .klass("frame")
        .add($("tr")
          .add($("td")
            .html(
              "<big>" +
              DateDm.fromStr(d[0]).toString() +
              " : [<font color='0041aa'>" +
              new Dec(d[1][0], 2).toIso() +
              "</font> | <font color='000000'>" +
              new Dec(d[1][1], 2).toIso() +
              "</font> | <font color='aa2800'>" +
              new Dec(d[1][2], 2).toIso() +
              "</font> | <font color='00aa41'>" +
              new Dec(d[1][0] - d[1][2], 2).toIso() +
              "</font>]</big>"))))
      .add($("div").klass("head").html(_("Last Month")))
      .add(lastMonthGr(data.slice(-30)))
      .add($("div").klass("head").html(_("Current Year")))
      .add(lastYearGr(data.filter(dv => dv[0] >= currentYear)))
      .add($("div").klass("head").html(_("Last Year")))
      .add(lastYearGr(data.filter(dv => dv[0] >= aYearAgo)))
      .add($("div").klass("head").html(_("All")))
      .add(allGr(data))
    ;

  }
}

