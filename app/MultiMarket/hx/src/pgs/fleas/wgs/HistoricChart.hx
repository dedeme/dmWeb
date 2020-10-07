// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.Dec;
import I18n._;

/// Historic quotes chart.
class HistoricChart {
  var isBig: Bool;
  var qs:  Array<HistoricChartEntry>;
  public final wg = Q("canvas");

  public function new (isBig: Bool, qs: Array<HistoricChartEntry>) {
    this.isBig = isBig;
    this.qs = qs;

    view();
  }


  // View ----------------------------------------------------------------------
  function view () {

    // Without data -----------------------------------------------------------
    if (qs.length <= 1) {
      var widthFrame = 260;
      var heightFrame = 150;
      var widthGr = 250;
      var heightGr = 140;
      if (isBig) {
        widthFrame = 610;
        heightFrame = 315;
        widthGr = 600;
        heightGr = 305;
      }

      final cv = wg
        .att("width", widthFrame)
        .att("height", heightFrame)
        .klass("GrFrame")
        .style("background:#d9d9d9");
      final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

      ctx.fillStyle = "rgba(255, 255, 255)";
      ctx.fillRect(5, 5, widthGr, heightGr);
      ctx.fillStyle = "rgba(0, 0, 0)";

      final tx = _("Without data");
      ctx.font = "32px sans";
      final text = ctx.measureText(tx);
      ctx.fillText(tx, (widthGr - text.width) / 2, heightGr / 2);
      return;
    }

    // Values -----------------------------------------------------------------

    if (!isBig) {
      qs = qs.slice(-250);
    }

    var qGood = qs[0].close;
    for (i in 0...qs.length)
      if (qs[i].close >= 0) {
        qGood = qs[i].close;
        break;
      }
    for (i in 0...qs.length)
      if (qs[i].close < 0) qs[i].close = qGood;
      else qGood = qs[i].close;

    final qsSize = qs.length;

    final lastQ = qs[qs.length - 1];
    final lastQ1 = qs[qs.length - 2];
    final backFrame =
      switch (lastQ.ref) {
        case Some(qref):
        switch (lastQ1.ref) {
          case Some(q1ref):
            (qref > lastQ.close && q1ref < lastQ1.close)
              ? "#f0f0ff"
              : (qref < lastQ.close && q1ref > lastQ1.close)
                ? "#fff0f0"
                : "#d9d9d9"
            ;
          case None: "#d9d9d9";
        }
        case None: "#d9d9d9";
      }

    final marginFrame = 5;
    final widthNumbers = isBig ? 60 : 40;
    final heightNumbers = isBig ? 25 : 20;

    final numberFont = isBig ? "12px sans" : "10px sans";
    final numberBaseLine = isBig ? 5 : 4;
    final numberHeight = isBig ? 12 : 10;

    final widthFrame = qsSize + 5 + 2 * marginFrame + widthNumbers;
    final heightFrame = (isBig ? 315 : 140) + 2 * marginFrame + heightNumbers;

    final x0Gr = marginFrame + widthNumbers + 0.5;
    final xnGr = widthFrame - marginFrame + 0.5;
    final widthGr = xnGr - x0Gr;

    final y0Gr = marginFrame + 0.5;
    final ynGr = heightFrame - heightNumbers + marginFrame + 0.5;
    final heightGr = ynGr - y0Gr;

    var max = qs[0].close;
    var min = max;
    for (e in qs) {
      if (e.close > max) max = e.close;
      if (e.ref != None && Opt.eget(e.ref) > max) max = Opt.eget(e.ref);
      if (e.close < min) min = e.close;
      if (e.ref != None && Opt.eget(e.ref) >= 0 && Opt.eget(e.ref) < min)
        min = Opt.eget(e.ref);
    }
    final dec = max >= 1000 || min <= -1000
      ? 0
      : max < 1 && min > -1
        ? 4
        : max < 10 && min > -10
          ? 3
          : 2
    ;
    final gap = max / 100;
    final base = Math.floor((min / gap) - 1) * gap;
    final top = Math.ceil(((max - min) / gap) + 2) * gap;
    final step = top / 4;

    // Rectangles -------------------------------------------------------------

    final cv = wg
      .att("width", widthFrame)
      .att("height", heightFrame)
      .klass("GrFrame")
      .style("background:" + backFrame)
    ;
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(x0Gr, y0Gr, widthGr, heightGr);
    ctx.fillStyle = "rgba(0, 0, 0)";

    // Left numbers -----------------------------------------------------------

    ctx.font = numberFont;
    for (i in 0...5) {
      final tx = Dec.toIso(base + step * i, dec);
      final text = ctx.measureText(tx);
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

    final xstep = 1;
    var x = x0Gr;
    ctx.beginPath();
    ctx.moveTo(x, ynGr - (qs[0].close - base) * heightGr / top);
    for (cs in qs) {
      final v = cs.close;
      final y = ynGr - (v - base) * heightGr / top;
      ctx.lineTo(x, y);
      x += xstep;
    }
    ctx.stroke();
    ctx.closePath();

    // Refs -------------------------------------------------------------------

    if (qs[0].ref != None) {
      x = x0Gr - 0.5;
      for (e in qs) {
        final v = Opt.eget(e.ref);
        if (v >= 0) {
          final y = ynGr - (v - base) * heightGr / top;
          if (v > e.close) {
            ctx.fillStyle = "rgba(0, 129, 255)";
          } else if (v < e.close) {
            ctx.fillStyle = "rgba(255, 40, 0)";
          }

          ctx.fillRect(x, y, 1, 1);
          ctx.fillStyle = "rgba(0, 0, 0)";
        }
        x += xstep;
      }
    }

    // Dates ------------------------------------------------------------------

    x = x0Gr - 0.5;
    var dPr = null;
    ctx.setLineDash([4, 2]);

    for (e in qs) {
      final d = e.date;
      if (dPr == null) {
        dPr = d;
        continue;
      }

      if (d.substring(0, 6) != dPr.substring(0, 6)) {
        dPr = d;
        if (Std.parseInt(d.substring(0, 6)) % 2 == 1) {
          ctx.beginPath();
          ctx.moveTo(x - 0.5, y0Gr);
          ctx.lineTo(x - 0.5, ynGr);
          ctx.stroke();
          ctx.closePath();

          final tx = d.substring(4, 6);
          final text = ctx.measureText(tx);
          ctx.fillText(
            tx,
            (x - text.width / 2),
            (ynGr + numberHeight)
          );
        }
      }

      x += xstep;
    };
    ctx.setLineDash([]);

    // Last rectangle ---------------------------------------------------------

    ctx.lineWidth = 1;
    ctx.strokeRect(x0Gr, y0Gr, widthGr, heightGr);
  }
}

class HistoricChartEntry {
  public var date: String;
  public var close: Float;
  public var ref: Option<Float>;

  public function new (date: String, close: Float, ?ref: Float) {
    this.date = date;
    this.close= close;
    this.ref = ref == null ? None : Some(ref);
  }
}
