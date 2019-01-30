// Copyright 04-Jan-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
//eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import ModalBox from "../dmjs/ModalBox.js";

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
 * @param {!Array<!Array<?>>} qs
 * @return {!Domo}
 */
function mkGr (qs) {
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

  let max = qs[0][0];
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
  let bs0 = 0;
  qs.forEach(cs => {
    const v = cs[1];
    const y = 145 - (v - base) * 140 / top;
    if (cs[1] > cs[0]) {
      if (bs0 === 2) {
        ctx.fillStyle = "rgba(255, 40, 0)";
      } else {
        ctx.fillStyle = "rgba(0, 129, 255)";
      }
      bs0 = 1;
    } else if (cs[1] < cs[0]) {
      if (bs0 === 1) {
        ctx.fillStyle = "rgba(0, 129, 255)";
      } else {
        ctx.fillStyle = "rgba(255, 40, 0)";
      }
      bs0 = 2;
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
 * @param {!Array<string>} ds
 * @param {!Array<!Array<number>>} qs
 */
function mkGrBig (box, div, nick, ds, qs) {
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

  let max = qs[0][0];
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
  let bs0 = 0;
  qs.forEach(cs => {
    const v = cs[1];
    const y = 290 - (v - base) * 280 / top;
    if (cs[1] > cs[0]) {
      if (bs0 === 2) {
        ctx.fillStyle = "rgba(255, 40, 0)";
      } else {
        ctx.fillStyle = "rgba(0, 129, 255)";
      }
      bs0 = 1;
    } else if (cs[1] < cs[0]) {
      if (bs0 === 1) {
        ctx.fillStyle = "rgba(0, 129, 255)";
      } else {
        ctx.fillStyle = "rgba(255, 40, 0)";
      }
      bs0 = 2;
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

function mkGrTd (nick, url, profits, qs) {
  const lastQ = qs[qs.length - 1];
  return $("td")
    .add($("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;width:40%").html(nick))
        .add($("td").add(led(url, lastQ[1] > lastQ[0] ? "#00AAFF" : "#FF8100")))
        .add($("td").style("text-align:right;width:40%").html(profits)))
      .add($("tr")
        .add($("td").att("colspan", 3)
          .add(mkGr(qs)))))
  ;
}

/** Companies page. */
export default class Companies {
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

  /** @private */
  formatN (n) {
    if (this._main.model["lang"] === "es") {
      return new Dec(n, 2).toEu();
    }
    return new Dec(n, 2).toEn();
  }

  /** @private */
  mkLink () {
    let value = true;
    let text = _("All Companies");
    if (this._main.model["allCos"]) {
      value = false;
      text = _("Portfolio");
    }
    const f = async () => {
      const rq = {
        "source": "companies",
        "rq": "allCos",
        "value": value
      };
      await this._main.client.send(rq);
      this._main.run();
    };
    return Ui.link(f).klass("link").html(text);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const tb = $("table").klass("frame").att("align", "center");
    const rs = $("div");
    const div = $("div");
    const box = new ModalBox(div);

    this._main.dom.show(Main.companiesPageId,
      $("div").style("text-align:center")
        .add($("div").add(this.mkLink()))
        .add(tb)
        .add(rs)
        .add(box.wg)
        .add(Ui.upTop("up"))
    );

    const rq = {
      "source": "companies",
      "rq": "list"
    };
    const rp = await this._main.client.send(rq);
    let list = rp["list"];
    if (!this._main.model["allCos"]) {
      list = list.filter(e => e[1]);
    }
    list = list.map(e => e[0]);
    list.sort();

    let tds = [];
    let ttProfits = 0;
    const ncols = 3;
    It.from(list).eachSync(
      async (nick) => {
        const rq = {
          "source": "companies",
          "rq": "historic",
          "nick": nick
        };
        const rp = await this._main.client.send(rq);
        const profits = rp["profits"];
        ttProfits += profits;
        const dates = rp["dates"];
        const quotes = rp["quotes"];
        const url = rp["url"];
        tds.push(
          mkGrTd(nick, url, this.formatN(profits), quotes)
            .on("click", () => {
              mkGrBig(box, div, nick, dates, quotes);
              box.show(true);
            })
        );
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
        rs.html(_("Profits") + ": " + this.formatN(ttProfits * 100) + "%");
      }
    );
  }
}
