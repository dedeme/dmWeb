// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.Js;
import dm.Dt;
import dm.DatePicker;
import I18n._;

/// Stocks page.
class StocksPage {
  static var wg: Domo;
  static var date: Date;

  // fs: Icon, Nick, mmStocks, mmPrice, stStocks, stPrice, stCost, hcCost
  //      0     1       2         3         4        5        6       7
  static function mkStocksEntry (fs: Array<String>): Domo {
    return Q("tr")
      .add(Q("td")
        .klass("border")
        .add(Ui.img(fs[0])))
      .add(Q("td")
        .klass("border")
        .style("text-align:center")
        .text(fs[1]))
      .add(Q("td"))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[2]))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[3]))
      .add(Q("td"))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[4]))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[5]))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[6]))
      .add(Q("td"))
      .add(Q("td")
        .klass("border")
        .style("text-align:right")
        .text(fs[7]))
    ;
  }

  // stocksEntry = Nick, stocks, price, cost
  static function missingKtMarket(stocksEntry: Array<String>): Domo {
    return mkStocksEntry([
      "error", stocksEntry[0],
      "- - -", "- - -",
      stocksEntry[1], stocksEntry[2], stocksEntry[3],
      ""
    ]);
  }

  // stocksEntry = Nick, stocks, price
  static function missingStocks(ktMarketEntry: Array<String>): Domo {
    return mkStocksEntry([
      "error", ktMarketEntry[0],
      ktMarketEntry[1], ktMarketEntry[2],
      "- - -", "- - -", "- - -",
      ""
    ]);
  }

  static function nickEntry(
    ktMarketEntry: Array<String>, stocksEntry: Array<String>
  ): Domo {
    final ok = ktMarketEntry[1] == stocksEntry[1] &&
      ktMarketEntry[2] == stocksEntry[2]
    ;

    return mkStocksEntry([
      ok ? "well" : "error", stocksEntry[0],
      ktMarketEntry[1], ktMarketEntry[2],
      stocksEntry[1], stocksEntry[2], stocksEntry[3],
      ""
    ]);
  }

  static function mkCash (
    div: Domo, ktMarket: String, hconta: String
  ): Void {
    final icon = ktMarket == hconta ? "well" : "error";
    div
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center;vertical-align:top;")
            .add(Q("div")
              .klass("head")
              .html(_("StocksCash"))))))
      .add(Q("table")
        .klass("summary")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .style("text-align:center")
            .text("·"))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text("KtMarket"))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text("Hconta")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(Ui.img(icon)))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(ktMarket))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(hconta))))

    ;
  }

  static function mkStocks (
    div: Domo,
    ktMarketStocks: Array<Array<String>>,
    stocksStocks: Array<Array<String>>,
    stocksSum: String,
    hcontaSum: String
  ): Void {
    function entries (): Array<Domo> {

      final r = [];
      var mmIx = 0;
      var stIx = 0;
      while (true) {
        if (mmIx >= ktMarketStocks.length && stIx >= stocksStocks.length) {
          break;
        }

        if (mmIx >= ktMarketStocks.length) {
          r.push(missingKtMarket(stocksStocks[stIx++]));
          continue;
        }

        if (stIx >= stocksStocks.length) {
          r.push(missingStocks(ktMarketStocks[mmIx++]));
          continue;
        }

        final mmNick = ktMarketStocks[mmIx][0];
        final stNick = stocksStocks[stIx][0];

        if (mmNick == stNick) {
          r.push(nickEntry(ktMarketStocks[mmIx++], stocksStocks[stIx++]));
        } else if (mmNick < stNick) {
          r.push(missingStocks(ktMarketStocks[mmIx++]));
        } else {
          r.push(missingKtMarket(stocksStocks[stIx++]));
        }
      }

      return r;
    }

    final dp = new DatePicker();
    dp.lang = I18n.lang;
    if (date != null) dp.date = date;
    dp.action = date -> {
      if (date == "") mk(wg)
      else changeDate(date);
    };

    div
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .style("padding-top:14px")
        .add(Q("tr")
          .add(Q("td")
            .add(dp.mkText(
                Q("input")
                .style("text-align:center;width:80px"))))))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center;vertical-align:top;")
            .add(Q("div")
              .klass("head")
              .html(_("Stocks"))))))
      .add(Q("table")
        .klass("summary")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("border"))
          .add(Q("td")
            .klass("border"))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .att("colspan", "2")
            .style("text-align:center")
            .text("KtMarket"))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .att("colspan", "3")
            .style("text-align:center")
            .text("Stocks"))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:center")
            .text("Hconta")))

        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .style("text-align:center")
            .text("·"))
          .add(Q("td")
            .klass("border")
            .style("text-align:center")
            .text("Nick"))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:75px;text-align:right")
            .text(_("Stks.")))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(_("Price")))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:75px;text-align:right")
            .text(_("Stks.")))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(_("Price")))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(_("Cost")))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(_("Cost"))))
        .adds(entries())
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "11")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .style("text-align:center")
            .add(Ui.img(stocksSum == hcontaSum ? "well" : "error")))
          .add(Q("td")
            .klass("border")
            .style("text-align:center")
            .text(_("Sum")))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:75px;text-align:right")
            .text(""))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(""))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:75px;text-align:right")
            .text(""))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(""))
          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(stocksSum))

          .add(Q("td"))

          .add(Q("td")
            .klass("border")
            .style("width:100px;text-align:right")
            .text(hcontaSum))))
    ;
  }

  /// Constructor.
  public static function mk (wg: Domo, ?date: String): Void {
    StocksPage.wg = wg;
    StocksPage.date = Date.now();
    if (date != null) {
      switch (Dt.from(date)) {
        case Some(d):
          final limit = DateTools.format(Date.now(), '%Y0101');
          if (date >= limit) StocksPage.date = d;
        case None:
      }
    }
    Cts.client.send([
      "source" => Js.ws("Stocks"),
      "rq" => Js.ws("idata"),
      "lastDate" => Js.ws(Dt.to(StocksPage.date))
    ], rp -> {

      final ktMarketCash = Dec.toIso(rp["ktMarketCash"].rf(), 2);
      final hcontaCash = Dec.toIso(rp["hcontaCash"].rf(), 2);

      final ktMarketStocks = rp["ktMarketStocks"].ra().map(rc -> {
          final f = rc.ra();
          return [
            f[0].rs(), // nick
            Dec.toIso(f[1].ri(), 0), // stocks
            Dec.toIso(f[2].rf(), 4)  // price
          ];
        });
      final stocksStocks = rp["stocksStocks"].ra().map(rc -> {
          final f = rc.ra();
          return [
            f[0].rs(), // nick
            Dec.toIso(f[1].ri(), 0), // stocks
            Dec.toIso(f[2].rf(), 4), // price
            Dec.toIso(f[3].rf(), 2)  // cost
          ];
        });

      final stocksSum = Dec.toIso(rp["stocksSum"].rf(), 2);
      final hcontaSum = Dec.toIso(rp["hcontaSum"].rf(), 2);

      ktMarketStocks.sort((e1, e2) -> {
        final s1 = e1[0].toUpperCase();
        final s2 = e2[0].toUpperCase();
        return s1 < s2 ? -1 : s1 > s2 ? 1 : 0;
      });

      stocksStocks.sort((e1, e2) -> {
        final s1 = e1[0].toUpperCase();
        final s2 = e2[0].toUpperCase();
        return s1 < s2 ? -1 : s1 > s2 ? 1 : 0;
      });

      final cashDiv = Q("div");
      final stocksDiv = Q("div");
      final sumDiv = Q("div");

      mkCash(cashDiv, ktMarketCash, hcontaCash);
      mkStocks(
        stocksDiv, ktMarketStocks, stocksStocks, stocksSum, hcontaSum
      );

      wg
        .removeAll()
        .add(cashDiv)
        .add(stocksDiv)
      ;
    });
  }

  // CONTROL

  static function changeDate (date: String): Void {
    final limit = DateTools.format(Date.now(), '%Y0101');
    if (date < limit) {
      Ui.alert(I18n._args(_("Date '%0' out of range (< %1)"), [date, limit]));
      mk(wg);
    } else {
      mk(wg, date);
    }
  }
}
