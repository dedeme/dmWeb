// Copyright 12-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Make an accounting chart
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Dec from "../../../dmjs/Dec.js";
import {_} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Msg from "../../../wgs/Msg.js";

const $ = e => Ui.$(e);

/**
    Removes values < 0 and lefts the last 250 values.
    @param {!Array<string>} dates
    @param {!Array<!Array<number>>} quotes
    @return void
**/
function regularize (dates, quotes) {
  function reg (qs) {
    let prev = 0;
    for (let i = 0; i < qs.length; ++i) {
      if (qs[i] >= 0) {
        prev = qs[i];
        break;
      }
    }

    for (let i = 0; i < qs.length; ++i) {
      if (qs[i] < 0) {
        qs[i] = prev;
      } else {
        prev = qs[i];
      }
    }

    quotes.splice(0, quotes.length - 250);
  }

  quotes.forEach(qs => { reg(qs) });
  dates.splice(0, dates.length - 250);
}

/**
    @param {!Array<number>} qs
    @return !Domo
**/
function backColor (qs1, qs) {
  let buy = false;
  let sell = false;
  for (let i = 1; i < qs.length; ++i) {
    if (qs1[i] > qs1[0] && qs[i] < qs[0]) buy = true;
    if (qs1[i] < qs1[0] && qs[i] > qs[0]) sell = true;
  }
  return buy && !sell
    ? "#ff8100"
    : !buy && sell
      ? "#00aaff"
      : buy && sell ? "#80ff80" : "#c9c9c9"
  ;
}

/**
    @param {number} price
    @param {!Array<!Array<number>>} quotes
    @return !Domo
**/
function mkSmallGr (price, quotes) {
  const len = quotes.length - 1;
  const backg = backColor(quotes[len - 1], quotes[len]);

  const cv = $("canvas")
    .att("width", 300)
    .att("height", 150)
    .klass("frame")
    .style("background:" + backg)
  ;
  const ctx = cv.e.getContext("2d");

  if (quotes.length <= 1) {
    const tx = _("Without data");
    ctx.font = "32px sans";
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (300 - text.width) / 2, 85);
    return cv;
  }

  let max = price > 0 ? price : quotes[0][0];
  let min = max;
  quotes.forEach(qs => {
    qs.forEach(v => {
      if (v > max) max = v;
      if (v < min) min = v;
    });
  });
  const gap = max / 100;
  const base = Math.floor((min / gap) - 1) * gap;
  const top = Math.ceil(((max - min) / gap) + 2) * gap;
  const step = top / 4;

  ctx.fillStyle = "rgba(255, 255, 255)";
  ctx.fillRect(45.5, 5.5, 252, 140);
  ctx.fillStyle = "rgba(0, 0, 0)";

  ctx.font = "10px sans";
  for (let i = 0; i < 5; ++i) {
    const tx = new Dec(base + step * i, 2).toIso();
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

  if (price > 0) {
    ctx.strokeStyle = "rgba(215, 215, 190)";
    const y = 145.5 - (price - base) * 140 / top;
    ctx.beginPath();
    ctx.moveTo(45.5, y);
    ctx.lineTo(294.5, y);
    ctx.stroke();
    ctx.closePath();
    ctx.strokeStyle = "rgba(0, 0, 0)";
  }

  const closes = quotes.map(qs => qs[0]);
  const xstep = 1;
  let x = 45.5;
  ctx.beginPath();
  ctx.moveTo(x, 145.5 - (closes[0] - base) * 140 / top);
  closes.forEach(v => {
    const y = 145.5 - (v - base) * 140 / top;
    ctx.lineTo(x, y);
    x += xstep;
  });
  ctx.stroke();
  ctx.closePath();

  for (let i = 1; i < quotes[0].length; ++i) {
    x = 45;
    for (let j = 0; j < quotes.length; ++j) {
      const cs = closes[j];
      const v = quotes[j][i];
      const y = 145 - (v - base) * 140 / top;
      if (v > cs) {
        ctx.fillStyle = "rgba(0, 129, 255)";
      } else if (v < cs) {
        ctx.fillStyle = "rgba(255, 40, 0)";
      }

      ctx.fillRect(x, y, 1, 1);
      ctx.fillStyle = "rgba(0, 0, 0)";
      x += xstep;
    }
  }

  ctx.lineWidth = 1;
  ctx.strokeRect(45.5, 5.5, 252, 140);

  return cv;
}

/**
    @param {string} nick
    @param {number} price
    @param {!Array<string>} dates
    @param {!Array<!Array<number>>} quotes
**/
function mkBigGr (nick, price, dates, quotes) {
  const len = quotes.length - 1;
  const backg = backColor(quotes[len - 1], quotes[len]);

  const cv = $("canvas")
    .att("width", 600)
    .att("height", 315)
    .klass("frame")
    .style("background:" + backg)
  ;
  const ctx = cv.e.getContext("2d");

  if (quotes.length <= 1) {
    const tx = _("Without data");
    ctx.font = "32px sans";
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (600 - text.width) / 2, 170);
    return;
  }

  let max = price > 0 ? price : quotes[0][0];
  let min = max;
  quotes.forEach(qs => {
    qs.forEach(v => {
      if (v > max) max = v;
      if (v < min) min = v;
    });
  });
  const gap = max / 100;
  const base = Math.floor((min / gap) - 1) * gap;
  const top = Math.ceil(((max - min) / gap) + 2) * gap;
  const step = top / 4;

  ctx.fillStyle = "rgba(255, 255, 255)";
  ctx.fillRect(90.5, 10.5, 504, 280);
  ctx.fillStyle = "rgba(0, 0, 0)";

  ctx.font = "12px sans";
  for (let i = 0; i < 5; ++i) {
    const tx = new Dec(base + step * i, 2).toIso();
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (80 - text.width), 295 - 70 * i);

    ctx.setLineDash([4, 2]);
    if (i > 0 && i < 4) {
      ctx.beginPath();
      ctx.moveTo(90.5, 290.5 - 70 * i);
      ctx.lineTo(588.5, 290.5 - 70 * i);
      ctx.stroke();
      ctx.closePath();
    }
    ctx.setLineDash([]);
  }

  if (price > 0) {
    ctx.strokeStyle = "rgba(215, 215, 190)";
    const y = 290.5 - (price - base) * 280 / top;
    ctx.beginPath();
    ctx.moveTo(90.5, y);
    ctx.lineTo(588.5, y);
    ctx.stroke();
    ctx.closePath();
    ctx.strokeStyle = "rgba(0, 0, 0)";
  }

  const xstep = 2;
  let x = 90.5;
  let previous = dates[0].substring(4, 6);
  dates.forEach(d => {
    const month = d.substring(4, 6);
    if (month !== previous) {
      previous = month;
      const text = ctx.measureText(month);
      ctx.fillText(month, (x - text.width), 305);
    }
    x += xstep;
  });

  const closes = quotes.map(qs => qs[0]);
  x = 90.5;
  ctx.beginPath();
  ctx.moveTo(x, 290.5 - (closes[0] - base) * 280 / top);
  closes.forEach(v => {
    const y = 290.5 - (v - base) * 280 / top;
    ctx.lineTo(x, y);
    x += xstep;
  });
  ctx.stroke();
  ctx.closePath();

  for (let i = 1; i < quotes[0].length; ++i) {
    x = 90.5;
    for (let j = 0; j < quotes.length; ++j) {
      const cs = closes[j];
      const v = quotes[j][i];
      const y = 290.5 - (v - base) * 280 / top;
      if (v > cs) {
        ctx.fillStyle = "rgba(0, 129, 255)";
      } else if (v < cs) {
        ctx.fillStyle = "rgba(255, 40, 0)";
      }

      ctx.fillRect(x, y, 2, 2);
      ctx.fillStyle = "rgba(0, 0, 0)";
      x += xstep;
    }
  }

  ctx.lineWidth = 1;
  ctx.strokeRect(90.5, 10.5, 504, 280);

  // ------------------

  const wg = $("table")
    .klass("main")
    .add($("tr")
      .add($("td")
        .style("text-align:right")
        .add($("span")
          .text("[ "))
        .add(Ui.link(() => { Msg.close() })
          .text(" X "))
        .add($("span")
          .text(" ]"))))
    .add($("tr")
      .add($("td")
        .style("text-align:center")
        .text(nick)))
    .add($("tr")
      .add($("td")
        .add(cv)))
  ;
  Msg.showWg(wg);
}

/**
    @param {string} url
    @param {!Array<number>} qs
    @return !Domo
**/
function led (url, qs) {
  const up = qs.reduce((r, q) => q > r ? q : r, qs[0]) === qs[0];
  const down = qs.reduce((r, q) => q < r ? q : r, qs[0]) === qs[0];
  const color = up && !down
    ? "#ff8100"
    : !up && down
      ? "#00aaff"
      : up && down ? "#c9c9c9" : "#80ff80"
  ;

  return $("div")
    .style("padding:5px;" +
           "border: 1px solid #002040;border-radius: 6px;" +
           "cursor:pointer;" +
           "background: " + color + ";")
    .on("click", (ev) => {
      window.open(url);
      ev.stopPropagation();
    })
  ;
}

/**
    @param {!Domo} wg
    @param {string} nick
    @param {string} url
    @param {number} price
    @param {number} profits
    @param {!Array<string>} dates
    @param {!Array<!Array<number>>} quotes
**/
function mkGr (wg, nick, url, price, profits, dates, quotes) {
  const lastQs = quotes[quotes.length - 1];
  wg
    .removeAll()
    .add($("table")
      .klass("main")
      .add($("tr")
        .add($("td")
          .style("text-align:left;width:40%")
          .html(nick))
        .add($("td")
          .add(led(url, lastQs)))
        .add($("td")
          .style("text-align:right;width:40%")
          .add($("span")
            .html(
              new Dec(profits, 2).toIso() + "&nbsp;&nbsp;"
            ))
          .add(Ui.img(
            profits > 0 ? "profits" : profits < 0 ? "losses" : "noresult"
          )
            .style("vertical-align:middle"))))
      .add($("tr")
        .add($("td")
          .att("colspan", 3)
          .add(mkSmallGr(price, quotes)
            .setStyle("cursor", "pointer")
            .on("click", () => {
              mkBigGr(
                nick, price, dates, quotes
              );
            })))))
  ;
}

/**
    Make an accounting chart
**/
export default class Chart {
  // Static --------------------------------------------------------------------

  /**
      @param {string} nick
      @param {string} url
      @return !Domo
  **/
  static mk (nick, url) {
    const wg = $("div");
    async function fn () {
      const rp = await Cts.client.send({
        "module": "acc",
        "source": "companies",
        "rq": "nickData",
        "nick": nick
      });
      const /** number */ price = rp["price"]; // -1 if nick is not in portfolio
      const /** number */ profits = rp["profits"];
      const /** !Array<string> */ dates = rp["dates"];
      // quotes[i][0] -> close; quotes[i][1...] -> references.
      const /** !Array<!Array<number>> */ quotes = rp["quotes"];
      regularize(dates, quotes);

      mkGr(wg, nick, url, price, profits, dates, quotes);
    }

    wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("text-align:center")
            .add(Ui.$("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;

    fn();
    return wg;
  }
}

