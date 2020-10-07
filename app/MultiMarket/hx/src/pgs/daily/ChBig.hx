// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;

/// Big chart
class ChBig {

  /// Constructor
  ///   hours : Hours in which quotes were recorded.
  ///   quotes: Quotes.
  ///   ratio : Current profits ratio to define chart background.
  public static function mk (
    hours: Array<Int>, quotes: Array<Float>, ratio: Float
  ): Domo {
    final cv = Q("canvas")
      .att("width", 305 * 2)
      .att("height", 160 * 2)
      .klass("frame")
    ;
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    final grGapX = 1.0 * 2;
    final grIncrX = 46.5 * 2;
    final grWidth = 250.0 * 2;
    final grIncrY = 5.5 * 2;
    final grHeight = 140.0 * 2;

    if (quotes.length <= 1) {
      final backg = "#f9f9f9";
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5 * 2.0, 0.5 * 2.0, 304 * 2.0, 159 * 2.0);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";
    } else {
      final backg = ratio > 0
        ? "#f0f0ff"
        : ratio < 0 ? "#fff0f0" : "#f9f9f9"
      ;
      cv.style("background-color:" + backg);
      ctx.fillStyle = backg;
      ctx.fillRect(0.5 * 2, 0.5 * 2, 304 * 2, 159 * 2);
      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(grIncrX - grGapX, grIncrY, grWidth + grGapX * 2, grHeight);
      ctx.fillStyle = "rgba(0, 0, 0)";

      var max = -1000000.0;
      var min = 1000000.0;
      for (q in quotes) {
        if (q > max) max = q;
        if (q < min) min = q;
      }

      final incr = (max - min) * 0.04;
      max += incr;
      min -= incr;
      final dif = max - min;

      function toGrY(n: Float): Float {
        return grIncrY + grHeight - (n - min) * grHeight / dif;
      }
      function toGrX(n: Float): Float {
        return n * grWidth / (quotes.length - 1) + grIncrX;
      }

      // Horizontal grid
      ctx.font = "12px sans";
      for (i in 0...5) {
        final y = min + dif * cast(i, Float) / 4.0;
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
      var lastHour = hours[0];
      ctx.setLineDash([4, 2]);
      for (i in 0...hours.length) {
        if (hours[i] != lastHour) {
          lastHour = hours[i];

          final tx = Dec.toIso(lastHour, 0);
          final text = ctx.measureText(tx);
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
      for (i in 0...quotes.length - 1) {
        ctx.beginPath();
        ctx.moveTo(toGrX(i), toGrY(quotes[i]));
        ctx.lineTo(toGrX(i + 1), toGrY(quotes[i + 1]));
        ctx.stroke();
        ctx.closePath();
      }
    }


    ctx.lineWidth = 1;
    ctx.strokeRect(90.5, 10.5, grWidth + grGapX * 2, grHeight);

    return Q("table").klass("main")
      .add(Q("tr")
        .add(Q("td")
          .add(cv)))
    ;
  }

}
