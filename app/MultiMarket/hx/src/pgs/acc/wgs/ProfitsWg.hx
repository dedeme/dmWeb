// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import dm.It;
import data.ProfitsEntry;
import I18n._;

/// Profits charts.

class ProfitsWg {
  static final ALL = 0;
  static final YEAR = ALL + 1;
  static final MONTH = YEAR + 1;

  static function mkCanvas(data: Array<ProfitsEntry>, type: Int): Domo {
    var backg = "#e9e9e9";
    final cv = Q("canvas").att("width", 600).att("height", 200)
      .klass("frame");
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    if (data.length <= 1) {
      final tx = _("Without data");
      ctx.font = "32px sans";
      final text = ctx.measureText(tx);
      ctx.fillText(tx, (600 - text.width) / 2, 100.0);
      return cv;
    }

    if (data[0].total < data[data.length - 1].total) {
      backg = "#e9e9f2";
    } else if (data[0].total > data[data.length - 1].total) {
      backg = "#f2e9e9";
    }
    cv.style("background:" + backg);

    var max = data[0].total;
    var min = max;
    for (dvs in data) {
      if (dvs.total > max) max = dvs.total;
      if (dvs.total < min) min = dvs.total;
      if (dvs.acc > max) max = dvs.acc;
      if (dvs.acc < min) min = dvs.acc;
      if (dvs.risk > max) max = dvs.risk;
      if (dvs.risk < min) min = dvs.risk;
    }
    final base = Math.floor((min / 1000) - 1) * 1000;
    final top = Math.ceil(((max - min) / 1000) + 2) * 1000;
    final step = top / 4;

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(100.5, 10.5, 490, 160);
    ctx.fillStyle = "rgba(0, 0, 0)";

    ctx.lineWidth = 1;
    ctx.strokeRect(100.5, 10.5, 490, 160);

    ctx.font = "12px sans";
    for (i in 0...5) {
      final tx = Dec.toIso(base + step * i, 0);
      final text = ctx.measureText(tx);
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

    final xstep = 488 / (data.length - 1);
    It.range(3).each(i -> {
      ctx.strokeStyle = i == 0 ? "rgba(0, 129, 255)"
        : i == 1 ? "rgba(0, 0, 0)"
          : "rgba(255, 40, 0)";
      var x = 100.5;
      ctx.beginPath();
      final v = i == 0
        ? data[0].total
        : i == 1
          ? data[0].acc
          : data[0].risk
      ;
      ctx.moveTo(x, 170.5 - (v - base) * 160 / top);
      for (dvs in data) {
        final v = i == 0
          ? dvs.total
          : i == 1
            ? dvs.acc
            : dvs.risk
        ;
        final y = 170.5 - (v - base) * 160 / top;
        ctx.lineTo(Math.floor(x) + 0.5, y);
        x += xstep;
      }
      ctx.stroke();
      ctx.closePath();
    });
    ctx.strokeStyle = "rgba(0, 0, 0)";

    var x = 100.5;
    var lastD = type == ALL ? data[0].date.substring(2, 4)
      : data[0].date.substring(4, 6);
    ctx.setLineDash([4, 2]);
    It.from(data).eachIx((dv, ix) -> {
      var d = type == ALL ? dv.date.substring(2, 4) : dv.date.substring(4, 6);
      if (
        (type != MONTH && lastD != d) ||
        (type == MONTH && ix != 0 && ix % 5 == 0)
      ) {
        if (type == MONTH) {
          d = dv.date.substring(6, 8);
        } else {
          lastD = d;
        }
        final x2 = Math.floor(x) + 0.5;
        final text = ctx.measureText(d);
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
  }

  static function lastMonthGr (data: Array<ProfitsEntry>): Domo {
    final cv = mkCanvas(data, MONTH);
    return Q("div").add(cv);
  };

  static function lastYearGr (data: Array<ProfitsEntry>): Domo {
    final cv = mkCanvas(data, YEAR);
    return Q("div").add(cv);
  };

  static function allGr (data: Array<ProfitsEntry>): Domo {
    final cv = mkCanvas(data, ALL);
    return Q("div").add(cv);
  };

  /// Constructor
  ///   wg  : Container.
  ///   data: Chart data.
  public static function mk (wg: Domo, data: Array<ProfitsEntry>): Void {
    if (data.length <= 1) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .klass("frame")
          .add(Q("tr")
            .add(Q("td")
              .html(_("Without data")))))
      ;
      return;
    }
    final d = data[data.length -  1];
    final aYearAgo = Dt.to(Dt.add(Date.now(), -365));
    final currentYear = DateTools.format(Date.now(), "%Y0101");
    wg
      .removeAll()
      .style("text-align:center")
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .html(
              "<big>" +
              Dt.toIso(Opt.eget(Dt.from(d.date))) +
              " : [<font color='0041aa'>" +
              Dec.toIso(d.total, 2) +
              "</font> | <font color='000000'>" +
              Dec.toIso(d.acc, 2) +
              "</font> | <font color='aa2800'>" +
              Dec.toIso(d.risk, 2) +
              "</font> | <font color='00aa41'>" +
              Dec.toIso(d.total - d.risk, 2) +
              "</font>]</big>"))))
      .add(Q("div").klass("head").html(_("Last Month")))
      .add(lastMonthGr(data.slice(-30)))
      .add(Q("div").klass("head").html(_("Current Year")))
      .add(lastYearGr(data.filter(dv -> dv.date >= currentYear)))
      .add(Q("div").klass("head").html(_("Last Year")))
      .add(lastYearGr(data.filter(dv -> dv.date >= aYearAgo)))
      .add(Q("div").klass("head").html(_("All")))
      .add(allGr(data))
    ;
  }
}


