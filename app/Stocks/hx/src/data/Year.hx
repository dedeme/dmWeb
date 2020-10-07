// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;
import dm.Dt;
import dm.It;
import dm.Dec;
import data.Report; // ReportSummary, ReportAnn
import I18n._;
import I18n._args;

/// Year data.
class Year {
  var nextId: Int;
  public var anns(default, null): Array<Ann>;
  public var error(default, null): String;
  // Report[0] is Report.WHIT_FEES,
  // Reports[1-Cts.investors] match investors[0:Cts.investors - 1] and
  // Report[Cts.investors + 1] is Report.ALL
  final reports: Array<Report> = [];

  /// Constructor.
  ///   nextId: Next identifier for annotations.
  ///   anns  : Year annotations. They will be sorted by date, from before to
  ///           after.
  public function new (nextId: Int, anns: Array<Ann>) {
    this.nextId = nextId;
    anns.sort((e1, e2) -> Dt.compare(e1.date, e2.date));
    this.anns = anns;

    error = "";

    // Check errors ------------------------------------------------------------

    final pf = new Map<String, Int>();
    for (a in anns) {
      final id = a.inv + "|" + a.nick;
      final s = pf.get(id);
      if (a.isSell) {
        final s = pf.get(id);
        if (s == null) {
          error = _args(
            _("%0\nNick %1 is unknown"),
            [a.toString(), a.nick]
          );
          break;
        }
        if (s < a.stocks) {
          error = _args(
            _("%0\nStock for sale (%1) > Stocks in portfolio (%2)"),
            [a.toString(), Std.string(a.stocks), Std.string(s)]
          );
          break;
        }
        pf[id] = s - a.stocks;
      } else {
        if (s == null) {
          pf[id] = a.stocks;
        } else {
          pf[id] = a.stocks + s;
        }
      }
    }

    // Reports -----------------------------------------------------------------

    final nRps = Cts.investors + 2;
    final profitss: Array<Float> = It.range(nRps).map(i -> 0.0).to();
    final summaries: Array<Array<ReportSummary>> =
      It.range(nRps).map(i -> []).to();
    final annss: Array<Array<ReportAnn>> =
      It.range(nRps).map(i -> []).to();
    var fees = 0.0;

    if (anns.length > 0) {
      final summs: Array<Map<String, ReportSummary>> =
        It.range(nRps).map(i -> new Map<String, ReportSummary>()).to();
      for (a in anns) {
        final nick = a.nick;
        profitss[nRps - 1] = 0.0; // profits of Report.ALL
        for (iRp in 0...nRps) {
          if (iRp > 0 && iRp < nRps - 1 && a.inv != iRp - 1) {
            continue;
          }
          final sentry = summs[iRp].get(nick);
          if (sentry == null) {
            if (!a.isSell) {
              final stocks = a.stocks;
              var price = a.price;
              var total = stocks * price;
              var afees = None;
              if (iRp == 0) {
                afees = Some(a.cash - total);
                fees += a.cash - total;
                total = a.cash;
                price = total / stocks;
              }
              annss[iRp].push(new ReportAnn(
                a.date, nick, stocks, price, total, None, afees
              ));

              summs[iRp][nick] = new ReportSummary(
                nick, stocks, price, total
              );
            }
          } else {
            if (a.isSell) {
              final stocks = a.stocks;
              var price = a.price;
              var total = stocks * price;
              var afees = None;
              if (iRp == 0) { // Report.WITH_FEES
                afees = Some(total - a.cash);
                fees += total - a.cash;
                total = a.cash;
                price = total / stocks;
              }

              var newStocks = sentry.stocks - stocks;
              var newTotal = 0.0;
              var newPrice = 0.0;
              var scost = sentry.total;
              if (newStocks < 0) {
                newStocks = 0;
                if (error == "") {
                  error = _args(
                    _("%0\nStock for sale (%1) > Stocks in portfolio (%2)"),
                    [a.toString(), Std.string(a.stocks),
                      Std.string(sentry.stocks)]
                  );
                }
              } else {
                scost = Dec.round(stocks * sentry.price, 2);
                newTotal = sentry.total - scost;
                newPrice = sentry.price;
              }

              final profits = total - scost;
              profitss[iRp] += profits;

              if (newStocks == 0) {
                summs[iRp].remove(nick);
              } else {
                summs[iRp][nick] = new ReportSummary(
                  nick, newStocks, newPrice, newTotal
                );
              }

              annss[iRp].push(new ReportAnn(
                a.date, nick, stocks, price, total, Some(profits), afees
              ));

            } else {
              final stocks = a.stocks;
              var price = a.price;
              var total = stocks * price;
              var afees = None;
              if (iRp == 0) { // Report.WITH_FEES
                afees = Some(a.cash - total);
                fees += a.cash - total;
                total = a.cash;
                price = total / stocks;
              }
              annss[iRp].push(new ReportAnn(
                a.date, nick, stocks, price, total, None, afees
              ));

              final newStocks = sentry.stocks + stocks;
              final newTotal = stocks * price + sentry.stocks * sentry.price;
              final newPrice = newTotal / newStocks;
              summs[iRp][nick] = new ReportSummary(
                nick, newStocks, newPrice, newTotal
              );
            }
          }
        }
      }

      // Recalculation of Report.ALL
      final allSumm = new Map<String, ReportSummary>();
      var allProfits = 0.0;
      for (i in 1...nRps - 1) {
        final summ = summs[i];
        for (k => v in summ) {
          final allV = allSumm[k];
          if (allV == null) {
            allSumm[k] = v;
          } else {
            final newStocks = v.stocks + allV.stocks;
            final newTotal = v.stocks * v.price + allV.stocks * allV.price;
            final newPrice = newTotal / newStocks;
            allSumm[k] = new ReportSummary(k, newStocks, newPrice, newTotal);
          }
        }
        allProfits += profitss[i];
      }
      summs[nRps - 1] = allSumm;
      profitss[nRps - 1] = allProfits;

      for (i in 0...nRps) {
        final sm: Array<ReportSummary> = [];
        for (k => v in summs[i]) sm.push(v);
        summaries[i] = sm;
      }
    }

    for (i in 0...nRps) {
      var cost = 0.0;
      for (e in summaries[i]){
        cost += e.total;
      }

      summaries[i].sort((e1, e2) -> e1.nick > e2.nick ? 1 : -1);
      reports.push(new Report(
        cost, profitss[i], i == 0 ? Some(fees) : None, summaries[i], annss[i]
      ));
    }
  }

  public function add(oldAnn: Option<Ann>, ann: Ann): Void {
    switch (oldAnn) {
      case Some(a):
        a.update(ann);
      case None:
        ann.id = nextId;
        nextId++;
        anns.push(ann);
    }
  }

  public function delete(annId: Int): Void {
    final ix = It.from(anns).indexf(e -> e.id == annId);
    if (ix != -1) {
      anns.splice(ix, 1);
    }
  }

  /// Makes a yearly report.
  ///   type: Report.ALL, Report.WITH_FEES or number of investor.
  public function report (type: Int): Report {
    return type == Report.ALL
      ? reports[Cts.investors + 1]
      : type == Report.WITH_FEES
        ? reports[0]
        : reports[type + 1]
    ;
  }

  /// Makes a treasury report.
  public function treasury (): {summary: Float, entries: Array<ReportAnn>} {
    final map = new Map<String, ReportAnn>();
    for (e in reports[0].anns) {
      if (e.profits == None) {
        continue;
      }
      final nick = e.nick;
      final emap = map.get(nick);
      if (emap == null) {
        map[nick] = e;
      } else {
        final stocks = emap.stocks + e.stocks;
        final total = emap.total + e.total;
        final profits = Opt.eget(emap.profits) + Opt.eget(e.profits);
        final fees = Opt.eget(emap.fees) + Opt.eget(e.fees);
        final price = total / stocks;
        map[nick] = new ReportAnn(
          e.date, nick, stocks, price, total, Some(profits), Some(fees)
        );
      }
    }
    var summary = 0.0;
    final entries: Array<ReportAnn> = [];
    for (v in map) {
      summary += Opt.eget(v.profits);
      entries.push(v);
    }
    entries.sort((e1, e2) -> e1.nick > e2.nick ? 1 : -1);

    return {summary: summary, entries: entries}
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(nextId),
      Js.wa(anns.map(e -> e.toJs()))
    ]);
  }

  // Static --------------------------------------------------------------------

  public static function fromJs (js: Js): Year {
    final a = js.ra();
    return new Year(
      a[0].ri(),
      a[1].ra().map(e -> Ann.fromJs(e))
    );
  }
}
