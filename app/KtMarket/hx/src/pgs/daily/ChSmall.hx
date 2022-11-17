// Copyright 20-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import data.DailyChart;

/// Small chart
class ChSmall {

  /// Constructor
  ///   inSelectPage: 'true' if chart is in selected page.
  ///   d: Chart data.
  ///   changeSel: Function to select-deselect a company.
  public static function mk (
    inSelectPage: Bool, d: DailyChart, changeSel: (Bool, String, Int) -> Void
  ): Domo {
    final nick = d.nick;
    final isSel = Selection.contains(nick);
    final close = d.close;
    final hs = d.hours;
    final qs = d.quotes;
    final quote = qs[qs.length - 1];
    final dif = (quote - close) / close;
    var dailyProfits = 0.0;
    var totalProfits = 0.0;
    var inPortfolio = false;

    if (qs[0] <= 0)
      throw new haxe.Exception('First quote of ${nick} is 0');

    // 0 = Not change, 1 = Sell, -1 = Buy
    var changeSellBuy = 0;
    // Tuples<Int, Float, Int>: [isBuy = (-1|-2)|isSell = (1|2), ref, manIx]
    final refs: Array<Array<Dynamic>> = [];
    // Pairs<Int, Float>: [isBuy = -1|isSell = 1, ref-quote %]
    final rs = [];
    for (manIx in 0...d.investorsData.length) {
      final e = d.investorsData[manIx];
      if (e.stocks > 0) {
        final total = e.stocks * (quote - e.price);
        final daily = e.todayBuy
          ? total
          : e.stocks * (quote - close)
        ;
        dailyProfits += daily;
        totalProfits += total;
        inPortfolio = true;
      }
      final isSold = e.ref < close ? 1 : -1;
      final dif = isSold == 1
        ? 1 - (quote - e.ref) / quote
        : 1 - (e.ref - quote) / e.ref
      ;
      if (dif > 1) {
        changeSellBuy = isSold;
      }
      var prevQ = qs.length > 0 ? qs[0]: 0;
      for (quote in qs) {
        final q = quote <= 0 ? prevQ : quote;
        prevQ = q;

        final dif = isSold == 1
          ? 1 - (q - e.ref) / q
          : 1 - (e.ref - q) / e.ref
        ;
        if (dif >= 0.98) {
          refs.push([isSold, e.ref, manIx]);
        } else if (e.stocks > 0 && e.modelId == "QFIJO" && !e.cought) {
          final ref2 = isSold == 1
            ? e.ref * Math.pow(1 + e.params[0], 1.5)
            : e.ref / Math.pow(1 + e.params[0], 1.5)
          ;
          final dif = isSold == -1
            ? 1 - (q - ref2) / q
            : 1 - (ref2 - q) / ref2
          ;

          if (dif >= 0.98) {
            refs.push([isSold * -2, ref2, manIx]);
          }
        }
      }
      rs.push([isSold, dif * 100]);
    }

    function separator (): Domo {
      return Q("span")
        .html("&nbsp;&nbsp;")
      ;
    }
    function color (v: Float): String {
      return "color:" + (v > 0 ? "#00aaff" : v < 0 ? "#ff8100" : "#a9a9a9");
    }
    function colorStocks (cought: Bool, v: Float, withStocks: Bool): String {
      return "color:" + (
        cought
        ? "#800000"
        : v > 0 ? "#00aaff"
          : v < 0 && withStocks ? "#ff8100"
            : "#a9a9a9"
      );
    }

    final rsWgs = [];
    for (i in 0...rs.length) {
      if (i > 0) {
        rsWgs.push(Q("span").text(
          Selection.investor(nick) == i && inSelectPage ? " | ·" : " | "
        ));
      } else {
        if (Selection.investor(nick) == 0 && inSelectPage) {
          rsWgs.push(Q("span").text("·"));
        }
      }
      if (inSelectPage) {
        rsWgs.push(Ui.link(() -> changeSel(true, nick, i))
          .style(colorStocks(
              d.investorsData[i].cought,
              -rs[i][0],
              d.investorsData[i].stocks > 0
            ) + ";cursor:pointer")
          .text(Dec.toIso(rs[i][1], 2) + "%")
        );
      } else {
        rsWgs.push(Q("span")
          .style(colorStocks(
              d.investorsData[i].cought,
              -rs[i][0],
              d.investorsData[i].stocks > 0
            ))
          .text(Dec.toIso(rs[i][1], 2) + "%")
        );
      }
    }

    final cv = Q("canvas")
      .att("width", 305)
      .att("height", 160)
      .klass("frame")
    ;
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    final grGapX = 1;
    final grIncrX = 46.5;
    final grWidth = 250;
    final grIncrY = 5.5;
    final grHeight = 140;

    if (qs.length <= 1) {
      final backg = "#f9f9f9";
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5, 0.5, 304, 159);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";
    } else {
      final backg = changeSellBuy == 1
        ? "#f0f0ff"
        : changeSellBuy == -1 ? "#fff0f0" : "#f9f9f9"
      ;
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5, 0.5, 304, 159);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";

      var max = -1000000.0;
      var min = 1000000.0;
      for (rf in refs) {
        if (rf[1] > max) max = rf[1];
        if (rf[1] < min) min = rf[1];
      }
      for (q in qs) {
        if (q <= 0) continue;
        if (q > max) max = q;
        if (q < min) min = q;
      }

      final incr = (max - min) * 0.04;
      max += incr;
      min -= incr;
      final dif = max - min;

      function toGrY (n: Float): Float {
        return grIncrY + grHeight - (n - min) * grHeight / dif;
      }
      function toGrX (n: Float): Float {
        return n * grWidth / (qs.length - 1) + grIncrX;
      }

      // Horizontal grid
      ctx.font = "10px sans";
      for (i in 0...5) {
        final y = min + dif * i / 4;
        final tx = y < 10 ? Dec.toIso(y, 3) : Dec.toIso(y, 2);
        final text = ctx.measureText(tx);
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
      var lastHour = hs[0];
      ctx.setLineDash([4, 2]);
      for (i in 0...hs.length) {
        if (hs[i] != lastHour) {
          lastHour = hs[i];

          final tx = Dec.toIso(lastHour, 0);
          final text = ctx.measureText(tx);
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
      for (i in 0...refs.length) {
        final rf = refs[i];
        ctx.strokeStyle = rf[0] > 0
          ? Cts.toBuyColors[rf[2]]
          : Cts.toSellColors[rf[2]]
        ;
        ctx.setLineDash([]);
        if (Math.abs(rf[0]) > 1) {
          ctx.setLineDash([5, 5]);
        }
        ctx.beginPath();
        ctx.moveTo(grIncrX - grGapX, toGrY(rf[1]));
        ctx.lineTo(grWidth + grIncrX + grGapX, toGrY(rf[1]));
        ctx.stroke();
        ctx.closePath();
      }
      ctx.strokeStyle = "rgba(0, 0, 0)";
      ctx.setLineDash([]);

      // Quotes
      for (i in 0...qs.length - 1) {
        var j = i;
        var q = qs[j];
        while (q <= 0) {
          --j;
          q = qs[j];
        }

        j = i + 1;
        var q1 = qs[j];
        while (q1 <= 0) {
          --j;
          q1 = qs[j];
        }

        ctx.beginPath();
        ctx.moveTo(toGrX(i), toGrY(q));
        ctx.lineTo(toGrX(i + 1), toGrY(q1));
        ctx.stroke();
        ctx.closePath();
      }
    }

    ctx.lineWidth = 1;
    ctx.strokeRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);

    return Q("table").klass("main")
      .add(Q("tr")
        .add(Q("td").klass("chartLeft")
          .add(Q("span")
            .text(nick))
          .add(separator())
          .add(isSel
            ? Ui.link(r -> changeSel(false, nick, -1))
              .add(Ui.img("unlink"))
            : Ui.link(e -> changeSel(true, nick, -1))
              .add(Ui.img("link")))
          .add(separator())
          .add(Q("span")
            .style("font-size:small")
            .text(Dec.toIso(quote, quote >= 10 ? 2 : 3)))
          .add(separator())
          .add(dif > 0
            ? Ui.img("money_plus")
            : dif < 0
              ? Ui.img("money_minus")
              : Ui.img("money"))
          .add(separator())
          .add(Q("span")
            .style("font-size:small;" + color(dif))
            .text(Dec.toIso(dif * 100, 2) + "%"))
          .add(Q("br"))
          .adds(rsWgs))
        .add(inPortfolio
          ? Q("td")
            .klass("chartRight")
            .add(Q("span")
              .style(color(dailyProfits))
              .text(Dec.toIso(dailyProfits, 2)))
            .add(Q("br"))
            .add(Q("span")
              .style(color(totalProfits))
              .text(Dec.toIso(totalProfits, 2)))
          : Q("td").klass("chartRight")))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(cv)))
    ;
  }
}
