// Copyright 24-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import AccMain from "./AccMain.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Domo from "../dmjs/Domo.js";  //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import ModalBox from "../dmjs/ModalBox.js";
import Operations from "../wgs/Operations.js";

const $ = Ui.$;

function led (url, color) {
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
 * @param {number} price
 * @param {!Array<!Array<?>>} qs
 * @return {!Domo}
 */
function mkGr (price, qs) {
  let backg = "#e9e9e9";
  const lastQ = qs[qs.length - 1];
  const lastQ1 = qs[qs.length - 2];
  if (lastQ[1] > lastQ[0] && lastQ1[1] < lastQ1[0]) {
    backg = "#f0f0ff";
  } else if (lastQ[1] < lastQ[0] && lastQ1[1] > lastQ1[0]) {
    backg = "#fff0f0";
  }
  const cv = $("canvas").att("width", 300).att("height", 150)
    .klass("frame").style("background:" + backg);
  const ctx = cv.e.getContext("2d");

  if (qs.length <= 1) {
    const tx = _("Without data");
    ctx.font = "32px sans";
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (300 - text.width) / 2, 85);
    return cv;
  }

  let max = price > 0 ? price : qs[0][0];
  let min = max;
  qs.forEach(dv => {
    let v = dv[0];
    if (v > max) max = v;
    if (v < min) min = v;
    v = dv[1];
    if (v > max) max = v;
    if (v < min) min = v;
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
    const tx = new Dec(base + step * i, 2).toEu();
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

  const xstep = 1;
  let x = 45.5;
  ctx.beginPath();
  ctx.moveTo(x, 145.5 - (qs[0][0] - base) * 140 / top);
  qs.forEach(cs => {
    const v = cs[0];
    const y = 145.5 - (v - base) * 140 / top;
    ctx.lineTo(x, y);
    x += xstep;
  });
  ctx.stroke();
  ctx.closePath();

  x = 45;
  qs.forEach(cs => {
    const v = cs[1];
    const y = 145 - (v - base) * 140 / top;
    if (cs[1] > cs[0]) {
      ctx.fillStyle = "rgba(0, 129, 255)";
    } else if (cs[1] < cs[0]) {
      ctx.fillStyle = "rgba(255, 40, 0)";
    }

    ctx.fillRect(x, y, 1, 1);
    ctx.fillStyle = "rgba(0, 0, 0)";
    x += xstep;
  });

  ctx.lineWidth = 1;
  ctx.strokeRect(45.5, 5.5, 252, 140);

  return cv;
}

/**
 * @param {!ModalBox} box
 * @param {!Domo} div
 * @param {string} nick
 * @param {number} refDay
 * @param {number} price
 * @param {!Array<string>} ds
 * @param {!Array<!Array<number>>} qs
 */
function mkGrBig (box, div, nick, refDay, price, ds, qs) {
  const bt = $("button").html("Close");

  let backg = "#e9e9e9";
  const lastQ = qs[qs.length - 1];
  const lastQ1 = qs[qs.length - 2];
  if (lastQ[1] > lastQ[0] && lastQ1[1] < lastQ1[0]) {
    backg = "#f0f0ff";
  } else if (lastQ[1] < lastQ[0] && lastQ1[1] > lastQ1[0]) {
    backg = "#fff0f0";
  }
  const cv = $("canvas").att("width", 600).att("height", 315)
    .klass("frame").style("background:" + backg);
  const ctx = cv.e.getContext("2d");

  if (qs.length <= 1) {
    const tx = _("Without data");
    ctx.font = "32px sans";
    const text = ctx.measureText(tx);
    ctx.fillText(tx, (600 - text.width) / 2, 170);
    return;
  }

  let max = price > 0 ? price : qs[0][0];
  let min = max;
  qs.forEach(dv => {
    let v = dv[0];
    if (v > max) max = v;
    if (v < min) min = v;
    v = dv[1];
    if (v > max) max = v;
    if (v < min) min = v;
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
    const tx = new Dec(base + step * i, 2).toEu();
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
  let previous = ds[0].substring(4, 6);
  ds.forEach(d => {
    const month = d.substring(4, 6);
    if (month !== previous) {
      previous = month;
      const text = ctx.measureText(month);
      ctx.fillText(month, (x - text.width), 305);
    }
    x += xstep;
  });

  x = 90.5;
  ctx.setLineDash([4, 2]);
  It.range(qs.length).each(i => {
    if (i === refDay) {
      ctx.beginPath();
      ctx.moveTo(x, 290.5);
      ctx.lineTo(x, 10);
      ctx.stroke();
      ctx.closePath();
    }
    x += xstep;
  });
  ctx.setLineDash([]);

  x = 90.5;
  ctx.beginPath();
  ctx.moveTo(x, 290.5 - (qs[0][0] - base) * 280 / top);
  qs.forEach(cs => {
    const v = cs[0];
    const y = 290.5 - (v - base) * 280 / top;
    ctx.lineTo(x, y);
    x += xstep;
  });
  ctx.stroke();
  ctx.closePath();

  x = 90;
  qs.forEach(cs => {
    const v = cs[1];
    const y = 290 - (v - base) * 280 / top;
    if (cs[1] > cs[0]) {
      ctx.fillStyle = "rgba(0, 129, 255)";
    } else if (cs[1] < cs[0]) {
      ctx.fillStyle = "rgba(255, 40, 0)";
    }

    ctx.fillRect(x, y, 2, 2);
    ctx.fillStyle = "rgba(0, 0, 0)";
    x += xstep;
  });

  ctx.lineWidth = 1;
  ctx.strokeRect(90.5, 10.5, 504, 280);

  div.removeAll()
    .add($("div").style("text-align:center").html(nick))
    .add(cv)
    .add($("hr"))
    .add(bt);
  bt.on("click", () => {
    box.show(false);
  });
}

function mkGrTd (box, div, nick, url, price, profits, ds, qs, hs) {
  const lastQ = qs[qs.length - 1];
  return $("td")
    .add($("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;width:40%").html(nick))
        .add($("td").add(led(url, lastQ[1] > lastQ[0] ? "#00AAFF" : "#FF8100")))
        .add($("td").style("text-align:right;width:40%")
          .add($("span")
            .html(
              new Dec(profits * 100, 2).toEu() + "%&nbsp;&nbsp;"
            ))
          .add(Ui.img(profits < 0 ? "loose" : "win")
            .style("vertical-align:top;")
            .on("click", () => {
              const bt = $("button").html("Close");
              div.removeAll()
                .add($("div").style("text-align:center").html(nick))
                .add(new Operations(hs).wg())
                .add($("hr"))
                .add(bt);
              bt.on("click", () => {
                box.show(false);
              });
              box.show(true);
            }))))
      .add($("tr")
        .add($("td").att("colspan", 3)
          .add(mkGr(price, qs)
            .setStyle("cursor", "pointer")
            .on("click", () => {
              mkGrBig(
                box, div, nick, qs.length, price, ds, qs
              );
              box.show(true);
            })))))
  ;
}

/** Companies page. */
export default class Companies {

  /**
   * @param {!AccMain} accMain Main
   */
  constructor (accMain) {
    this._accMain = accMain;
    /** @type{string} */
    this._lang = accMain.lang;
    /** @type{boolean} */
    this._allCompanies = false;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  formatN (n) {
    if (this._lang === "es") {
      return new Dec(n, 2).toEu();
    }
    return new Dec(n, 2).toEn();
  }

  /**
   * @private
   * @param {boolean} allCos
   * @return {!Domo}
   */
  mkLink (allCos) {
    let value = true;
    let text = _("All Companies");
    if (allCos) {
      value = false;
      text = _("Portfolio");
    }
    const f = async () => {
      const rq = {
        "module": "acc",
        "source": "Companies",
        "rq": "setAllCos",
        "value": value
      };
      await Main.client.rq(rq);
      this.show();
    };
    return Ui.link(f).klass("link").html(text);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const tb = $("table").klass("frame").att("align", "center");
    const lk = $("div");
    const rs = $("div");
    const div = $("div");
    const box = new ModalBox(div);

    this._accMain.view.removeAll().add(
      $("div").style("text-align:center")
        .add(lk)
        .add(rs)
        .add(tb)
        .add(box.wg)
        .add(Ui.upTop("up"))
    );

    const rq = {
      "module": "acc",
      "source": "Companies",
      "rq": "nicks"
    };
    const rp = await Main.client.rq(rq);
    const allCos = rp["allCos"];
    const nicks = rp["nicks"];

    lk.removeAll().add(this.mkLink(allCos));

    nicks.sort();

    let tds = [];
    let ttProfits = 0;
    const ncols = 3;
    It.from(nicks).eachSync(
      async (nick) => {
        const rq = {
          "module": "acc",
          "source": "Companies",
          "rq": "historic",
          "nick": nick
        };
        const rp = await Main.client.rq(rq);

        const price = rp["price"];
        const profits = rp["profits"];
        ttProfits += profits;
        const dates = rp["dates"];
        const quotes = rp["quotes"];
        const url = rp["url"];
        const historic = rp["historic"];
        tds.push(mkGrTd(
          box, div, nick, url, price, profits, dates, quotes, historic
        ));
        if (tds.length === ncols) {
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tb.add($("tr").adds(tds));
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tds = [];
        }
      },
      () => {
        if (tds.length > 0) {
          It.range(ncols - tds.length).each(() => { tds.push($("td")) });
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tb.add($("tr").adds(tds));
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
        }
        if (nicks.length > 0) {
          ttProfits = ttProfits / nicks.length;
        }
        rs.html(_("Profits") + ": " + this.formatN(ttProfits * 100) + "%");
      }
    );
  }

}

