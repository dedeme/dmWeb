// Copyright 18-Sep-2020 ºDeme
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
  ///   d: Chart data.
  ///   changeSel: Function to select-deselect a company.
  public static function mk (
    d: DailyChart, changeSel: (Bool, String) -> Void
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
    // 0 = Not change, 1 = Sell, -1 = Buy
    var changeSellBuy = 0;
    // Pairs<number, number>: [isBuy = -1|isSell = 1, ref]
    final refs = [];
    // Pairs<number, number>: [isBuy = -1|isSell = 1, ref-quote %]
    final rs = d.managersData.map(e -> {
      var daily = 0.0;
      if (e.stocks > 0) {
        daily = e.stocks * (quote - close);
        dailyProfits += daily;
        totalProfits += e.stocks * (quote - e.price);
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
      for (q in qs) {
        final dif = isSold == 1
          ? 1 - (q - e.ref) / q
          : 1 - (e.ref - q) / e.ref
        ;
        if (dif >= 0.98) {
          refs.push([isSold, e.ref]);
        }
      }
      return [isSold, dif * 100];
    });

    function separator (): Domo {
      return Q("span")
        .html("&nbsp;&nbsp;")
      ;
    }
    function color (v: Float): String {
      return "color:" + (v > 0 ? "#00aaff" : v < 0 ? "#ff8100" : "#c9c9c9");
    }

    final rsWgs = [];
    for (i in 0...rs.length) {
      if (i > 0) {
        rsWgs.push(Q("span").text(" | "));
      }
      rsWgs.push(Q("span")
        .style(color(-rs[i][0]))
        .text(Dec.toIso(rs[i][1], 2) + "%")
      );
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
        final tx = Dec.toIso(y, 2);
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
      for (rf in refs) {
        ctx.strokeStyle = rf[0] == 1
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
      for (i in 0...qs.length - 1) {
        ctx.beginPath();
        ctx.moveTo(toGrX(i), toGrY(qs[i]));
        ctx.lineTo(toGrX(i + 1), toGrY(qs[i + 1]));
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
            ? Ui.link(r -> changeSel(false, nick))
              .add(Ui.img("unlink"))
            : Ui.link(e -> changeSel(true, nick))
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
