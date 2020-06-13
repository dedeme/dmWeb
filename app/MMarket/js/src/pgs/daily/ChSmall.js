// Copyright 08-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Dec from "../../dmjs/Dec.js";
import Ui from "../../dmjs/Ui.js";
import {DailyChart} from "../../data/DailyChart.js"; //eslint-disable-line
import Selection from "./Selection.js";

const $ = e => Ui.$(e);

/** Small chart. */
export default class ChSmall {
  // Static --------------------------------------------------------------------
  /**
      @param {!DailyChart} d
      @param {function(boolean, string):void} changeSel
      @return !Domo
  **/
  static mk (d, changeSel) {
    const nick = d.nick;
    const isSel = Selection.contains(nick);
    const close = d.close;
    const hours = d.hours;
    const quotes = d.quotes;
    const quote = quotes[0];
    const dif = (quote - close) / close;
    let dailyProfits = 0;
    let totalProfits = 0;
    let inPortfolio = false;
    // 0 = Not change, 1 = Sell, -1 = Buy
    let changeSellBuy = 0;
    // Pairs<number, number>: [isBuy = -1|isSell = 1, ref]
    const refs = [];
    // Pairs<number, number>: [isBuy = -1|isSell = 1, ref-quote %]
    const rs = d.accData.map(e => {
      let daily = 0;
      if (e.stocks > 0) {
        daily = e.stocks * (quote - close);
        dailyProfits += daily;
        totalProfits += e.stocks * (quote - e.price);
        inPortfolio = true;
      }
      const isSold = e.ref < close ? 1 : -1;
      const dif = isSold === 1
        ? 1 - (quote - e.ref) / quote
        : 1 - (e.ref - quote) / e.ref
      ;
      if (dif > 1) {
        changeSellBuy = isSold;
      }
      for (const q of quotes) {
        const dif = isSold === 1
          ? 1 - (q - e.ref) / q
          : 1 - (e.ref - q) / e.ref
        ;
        if (dif >= 0.98) {
          refs.push([isSold, e.ref]);
        }
      }
      return [isSold, dif * 100];
    });

    function separator () {
      return $("span")
        .html("&nbsp;&nbsp;")
      ;
    }
    function color (v) {
      return "color:" + (v > 0 ? "#00aaff" : v < 0 ? "#ff8100" : "#c9c9c9");
    }

    const rsWgs = [];
    for (let i = 0; i < rs.length; ++i) {
      if (i > 0) {
        rsWgs.push($("span").text(" | "));
      }
      rsWgs.push($("span")
        .style(color(-rs[i][0]))
        .text(new Dec(rs[i][1], 2).toIso() + "%")
      );
    }

    const hs = hours.map(e => e);
    hs.reverse();
    const qs = quotes.map(e => e);
    qs.reverse();
    const cv = $("canvas")
      .att("width", 305)
      .att("height", 160)
      .klass("frame")
    ;
    const ctx = cv.e.getContext("2d");

    const grGapX = 1;
    const grIncrX = 46.5;
    const grWidth = 250;
    const grIncrY = 5.5;
    const grHeight = 140;

    if (qs.length <= 1) {
      const backg = "#f9f9f9";
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5, 0.5, 304, 159);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";
    } else {
      const backg = changeSellBuy === 1
        ? "#f0f0ff"
        : changeSellBuy === -1 ? "#fff0f0" : "#f9f9f9"
      ;
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5, 0.5, 304, 159);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";

      let max = -1000000;
      let min = 1000000;
      for (const rf of refs) {
        if (rf[1] > max) max = rf[1];
        if (rf[1] < min) min = rf[1];
      }
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
      ctx.font = "10px sans";
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
            grIncrY + grHeight + 10
          );

          ctx.beginPath();
          ctx.moveTo(toGrX(i), grIncrY);
          ctx.lineTo(toGrX(i), grIncrY + grHeight);
          ctx.stroke();
          ctx.closePath();
        }
      }
      ctx.setLineDash([]);

      // Refs
      for (const rf of refs) {
        ctx.strokeStyle = rf[0] === 1
          ? "rgba(255, 129, 0)"
          : "rgba(0, 170, 255)"
        ;
        ctx.beginPath();
        ctx.moveTo(grIncrX - grGapX, toGrY(rf[1]));
        ctx.lineTo(grWidth + grIncrX + grGapX, toGrY(rf[1]));
        ctx.stroke();
        ctx.closePath();
      }
      ctx.strokeStyle = "rgba(0, 0, 0)";

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
    ctx.strokeRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);

    return $("table").klass("main")
      .add($("tr")
        .add($("td").klass("chartLeft")
          .add($("span")
            .text(nick))
          .add(separator())
          .add(isSel
            ? Ui.link(() => { changeSel(false, nick) })
              .add(Ui.img("unlink"))
            : Ui.link(() => { changeSel(true, nick) })
              .add(Ui.img("link")))
          .add(separator())
          .add($("span")
            .style("font-size:small")
            .text(new Dec(quote, quote >= 10 ? 2 : 3).toIso()))
          .add(separator())
          .add(dif > 0
            ? Ui.img("money_plus")
            : dif < 0
              ? Ui.img("money_minus")
              : Ui.img("money"))
          .add(separator())
          .add($("span")
            .style("font-size:small;" + color(dif))
            .text(new Dec(dif * 100, 2).toIso() + "%"))
          .add($("br"))
          .adds(rsWgs))
        .add(inPortfolio
          ? $("td")
            .klass("chartRight")
            .add($("span")
              .style(color(dailyProfits))
              .text(new Dec(dailyProfits, 2).toIso()))
            .add($("br"))
            .add($("span")
              .style(color(totalProfits))
              .text(new Dec(totalProfits, 2).toIso()))
          : $("td").klass("chartRight")))
      .add($("tr")
        .add($("td")
          .att("colspan", 2)
          .add(cv)))
    ;
  }


}
