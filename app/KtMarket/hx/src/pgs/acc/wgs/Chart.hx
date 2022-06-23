// Copyright 20-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import wgs.Msg;
import I18n._;

/// Make an accounting chart
class Chart {
  /**
      @param {string} nick
      @param {string} url
      @return !Domo
  **/
  /// Constructor:
  ///   onPorfolio: 'true' if only shows data of sold values.
  ///   nick   : Company nick.
  ///   url    : Company url to amplify information.
  public static function mk (
    onPortfolio: Bool, nick: String, url: String
  ): Domo {
    final wg = Q("div");
    function fn (): Void {
      Cts.client.send([
        "module" => Js.ws("acc"),
        "source" => Js.ws("companies"),
        "rq" => Js.ws("nickData"),
        "nick" => Js.ws(nick)
      ], rp -> {
        // Number of investors
        final invsN = rp["investorsN"].ri();
        // Price if nick is in portfolio, otherwise -1.
        final price = rp["price"].rf();
        // Array of investors which have got the company.
        final invs = rp["investors"].ra().map(e -> e.ri());
        // Profits if nick is in portfolio, otherwise 0.
        final profits = rp["profits"].rf();
        // Dates of 'quotes'
        final dates = rp["dates"].ra().map(e -> e.rs());
        // Array of [dates][cts.qlevels+1]values.
        //  [i][0] -> close
        //  [i][j+1] -> reference of investor 'j'
        // Example with 3 investors:
        //    |  cl  |  r1  |  r2  |  r3  |
        //    | 2.52 | 2.60 | 2.10 | 2.22 |
        //    | 2.55 | 2.60 | 2.15 | 2.23 |
        //    ...
        final quotes = rp["quotes"].ra()
          .map(row -> row.ra().map(e -> e.rf()));
        regularize(dates, quotes);

        mkGrs(
          wg, invsN, onPortfolio, nick, url, price, invs, profits, dates, quotes
        );
      });
    }

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.Q("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;

    fn();
    return wg;
  }

  /// Removes values < 0 and lefts the last 250 values.
  static function regularize (
    dates: Array<String>, quotes: Array<Array<Float>>
  ): Void {
    function reg (qs: Array<Float>): Void {
      var prev = 0.0;
      for (i in 0...qs.length) {
        if (qs[i] >= 0) {
          prev = qs[i];
          break;
        }
      }

      for (i in 0...qs.length) {
        if (qs[i] < 0) {
          qs[i] = prev;
        } else {
          prev = qs[i];
        }
      }

      quotes.splice(0, quotes.length - 250);
    }

    for (qs in quotes) reg(qs);
    dates.splice(0, dates.length - 250);
  }

  static function backColor (qs1: Array<Float>, qs: Array<Float>): String {
    var buy = false;
    var sell = false;
    for (i in 1...qs.length) {
      if (qs1[i] > qs1[0] && qs[i] < qs[0]) buy = true;
      if (qs1[i] < qs1[0] && qs[i] > qs[0]) sell = true;
    }
    return buy && !sell
      ? "#fff0f0"
      : !buy && sell
        ? "#f0f0ff"
        : buy && sell ? "#f0fff0" : "#c9c9c9"
    ;
  }

  static function mkSmallGr (
    onPortfolio: Bool, price: Float, invs: Array<Int>,
    quotes: Array<Array<Float>>
  ): Domo {
    final len = quotes.length - 1;
    final backg = backColor(quotes[len - 1], quotes[len]);

    final cv = Q("canvas")
      .att("width", 300)
      .att("height", 150)
      .klass("frame")
      .style("background:" + backg)
    ;
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    if (quotes.length <= 1) {
      final tx = _("Without data");
      ctx.font = "32px sans";
      final text = ctx.measureText(tx);
      ctx.fillText(tx, (300 - text.width) / 2, 85);
      return cv;
    }

    var max = price > 0 ? price : quotes[0][0];
    var min = max;
    for (qs in quotes) {
      for (v in qs) {
        if (v > max) max = v;
        if (v < min) min = v;
      }
    }
    final gap = max / 100;
    final base = Math.floor((min / gap) - 1) * gap;
    final top = Math.ceil(((max - min) / gap) + 2) * gap;
    final step = top / 4;

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(45.5, 5.5, 252, 140);
    ctx.fillStyle = "rgba(0, 0, 0)";

    ctx.font = "10px sans";
    for (i in 0...5) {
      final tx = Dec.toIso(base + step * i, 2);
      final text = ctx.measureText(tx);
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
      final y = 145.5 - (price - base) * 140 / top;
      ctx.beginPath();
      ctx.moveTo(45.5, y);
      ctx.lineTo(294.5, y);
      ctx.stroke();
      ctx.closePath();
      ctx.strokeStyle = "rgba(0, 0, 0)";
    }

    final closes = quotes.map(qs -> qs[0]);
    final xstep = 1;
    var x = 45.5;
    ctx.beginPath();
    ctx.moveTo(x, 145.5 - (closes[0] - base) * 140 / top);
    for (v in closes) {
      final y = 145.5 - (v - base) * 140 / top;
      ctx.lineTo(x, y);
      x += xstep;
    }
    ctx.stroke();
    ctx.closePath();

    for (i in 1...quotes[0].length) {
      if (onPortfolio && !invs.contains(i - 1)) {
        continue;
      }
      x = 45;
      for (j in 0...quotes.length) {
        final cs = closes[j];
        final v = quotes[j][i];
        final y = 145 - (v - base) * 140 / top;
        if (v > cs) {
          ctx.fillStyle = Cts.toSellColors[i - 1];
        } else if (v < cs) {
          ctx.fillStyle = Cts.toBuyColors[i - 1];
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

  static function mkBigGr (
    onPortfolio: Bool, nick: String, price: Float, invs: Array<Int>,
    dates: Array<String>, quotes: Array<Array<Float>>
  ): Void {
    final len = quotes.length - 1;
    final backg = backColor(quotes[len - 1], quotes[len]);

    final cv = Q("canvas")
      .att("width", 600)
      .att("height", 315)
      .klass("frame")
      .style("background:" + backg)
    ;
    final ctx = cast(cv.e, js.html.CanvasElement).getContext("2d");

    if (quotes.length <= 1) {
      final tx = _("Without data");
      ctx.font = "32px sans";
      final text = ctx.measureText(tx);
      ctx.fillText(tx, (600 - text.width) / 2, 170);
      return;
    }

    var max = price > 0 ? price : quotes[0][0];
    var min = max;
    for (qs in quotes) {
      for (v in qs) {
        if (v > max) max = v;
        if (v < min) min = v;
      }
    }
    final gap = max / 100;
    final base = Math.floor((min / gap) - 1) * gap;
    final top = Math.ceil(((max - min) / gap) + 2) * gap;
    final step = top / 4;

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(90.5, 10.5, 504, 280);
    ctx.fillStyle = "rgba(0, 0, 0)";

    ctx.font = "12px sans";
    for (i in 0...5) {
      final tx = Dec.toIso(base + step * i, 2);
      final text = ctx.measureText(tx);
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
      final y = 290.5 - (price - base) * 280 / top;
      ctx.beginPath();
      ctx.moveTo(90.5, y);
      ctx.lineTo(588.5, y);
      ctx.stroke();
      ctx.closePath();
      ctx.strokeStyle = "rgba(0, 0, 0)";
    }

    final xstep = 2;
    var x = 90.5;
    var previous = dates[0].substring(4, 6);
    for (d in dates) {
      final month = d.substring(4, 6);
      if (month != previous) {
        previous = month;
        final text = ctx.measureText(month);
        ctx.fillText(month, (x - text.width), 305);
      }
      x += xstep;
    }

    final closes = quotes.map(qs -> qs[0]);
    x = 90.5;
    ctx.beginPath();
    ctx.moveTo(x, 290.5 - (closes[0] - base) * 280 / top);
    for (v in closes) {
      final y = 290.5 - (v - base) * 280 / top;
      ctx.lineTo(x, y);
      x += xstep;
    }
    ctx.stroke();
    ctx.closePath();

    for (i in 1...quotes[0].length) {
      if (onPortfolio && !invs.contains(i - 1)) {
        continue;
      }
      x = 90.5;
      for (j in 0...quotes.length) {
        final cs = closes[j];
        final v = quotes[j][i];
        final y = 290.5 - (v - base) * 280 / top;
        if (v > cs) {
          ctx.fillStyle = Cts.toSellColors[i - 1];
        } else if (v < cs) {
          ctx.fillStyle = Cts.toBuyColors[i - 1];
        }
        ctx.fillRect(x, y, 2, 2);
        ctx.fillStyle = "rgba(0, 0, 0)";
        x += xstep;
      }
    }

    ctx.lineWidth = 1;
    ctx.strokeRect(90.5, 10.5, 504, 280);

    // ------------------

    final wg = Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:center")
          .text(nick)))
      .add(Q("tr")
        .add(Q("td")
          .add(cv)))
    ;
    Msg.showWg(wg);
  }

  static function leds(invsN: Int, invs: Array<Int>): Domo {
    return Q("table")
      .style("border-collapse : collapse;")
      .add(Q("tr")
        .adds(It.range(invsN).map(inv ->
          Q("td")
            .add(Q("div")
              .style("padding:5px;" +
               "border: 1px solid #002040;border-radius: 6px;" +
               "cursor:pointer;" +
               "background: " +
               (invs.contains(inv)? Cts.toBuyColors[inv] : "#e9e9e9") +
               ";")))))
    ;
  }

  static function mkGrs (
    wg: Domo, invsN: Int, onPortfolio: Bool,
    nick: String, url: String, price: Float, invs: Array<Int>, profits: Float,
    dates: Array<String>, quotes: Array<Array<Float>>
  ) {
    final lastQsx = quotes[quotes.length - 1];
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left;width:40%")
            .add(Q("a")
              .klass("link")
              .att("href", url)
              .text(nick)))
          .add(Q("td")
            .add(leds(invsN, invs)))
          .add(Q("td")
            .style("text-align:right;width:40%")
            .add(Q("span")
              .html(Dec.toIso(profits, 2) + "&nbsp;&nbsp;"))
            .add(Ui.img(
              profits > 0 ? "profits" : profits < 0 ? "losses" : "noresult"
            )
              .style("vertical-align:middle"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(mkSmallGr(onPortfolio, price, invs, quotes)
              .setStyle("cursor", "pointer")
              .on(CLICK, e -> mkBigGr(
                  onPortfolio, nick, price, invs, dates, quotes
                ))))))
    ;
  }

}
