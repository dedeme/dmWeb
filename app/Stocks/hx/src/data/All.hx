// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import dm.Str;
import data.FormRow;
import I18n._;
import I18n._args;

/// All the years data.
class All {
  public var data(default, null): Map<String, Year>;

  function new (data: Map<String, Year>) {
    this.data = data;
  }

  /// Sends data to server.
  /// If data is out of date no changes is made and a "expired" page is shown.
  public function send (fn: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("write"),
      "data" => this.toJs()
    ], rp -> {
      fn();
    });
  }

  /// Returns last year identifier.
  public function lastYearId (): String {
    var year = "";
    for (k => v in data) {
      if (k > year) {
        year = k;
      }
    }
    return year;
  }

  /// Returns a sorted list of year identifiers.
  public function yearIds (): Array<String> {
    final r = [];
    for (k in data.keys()) {
      r.push(k);
    }
    r.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    return r;
  }

  /// Returns 'true' if nicks allreade exists.
  public function duplicateNick (nick: String): Bool {
    for (v in data) {
      for (e in v.anns) {
        if (e.nick == nick)
          return true;
      }
    }
    return false;
  }

  /// Returns a sorted list of nicks for an investor and a year.
  ///   type: Report.ALL, Report.WITH_FEES or number of investor.
  ///   year: Year of report. If its value is None, report is of every year.
  public function nicks (type: Int, year: Option<String>): Array<String> {
    final mp = new Map<String, String>();
    for (k => v in data) {
      switch (year) {
        case Some(y): if (y != k) continue;
        case None:
      }
      for (e in v.anns) {
        if (type >= 0 && e.inv != type) {
          continue;
        }
        mp[e.nick] = e.nick;
      }
    }
    final r = [];
    for (v in mp) {
      r.push(v);
    }
    r.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    return r;
  }

  /// Returns form of a nick.
  ///   type: Report.ALL, Report.WITH_FEES or number of investor.
  ///   nick: Nick to show.
  ///   year: Year of report. If its value is None, report is of every year.
  public function form (
    type: Int, nick: String, year: Option<String>
  ): Array<FormRow> {
    final r = [];
    for (k => v in data) {
      switch (year) {
        case Some(y): if (y != k) continue;
        case None:
      }
      for (e in v.anns) {
        if ((type >= 0 && e.inv != type) || e.nick != nick) {
          continue;
        }

        if (r.length == 0) {
          final stocks = e.stocks;
          var price = e.price;
          var total = stocks * price;
          var fees = None;
          if (type == Report.WITH_FEES) {
            fees = Some(Math.abs(total - e.cash));
            total = e.cash;
            price = total / stocks;
          }
          if (e.isSell) {
            r.push(new FormRow(
              Dt.toIso(e.date),
              0, 0.0, 0.0,
              stocks, price, total,
              0, 0, 0,
              None, 0.0,
              fees, switch (fees) {case None: 0; case Some(v): v;}
            ));
          } else {
            r.push(new FormRow(
              Dt.toIso(e.date),
              stocks, price, total,
              0, 0.0, 0.0,
              stocks, price, total,
              None, 0.0,
              fees, switch (fees) {case None: 0; case Some(v): v;}
            ));
          }
        } else {
          if (year == None && Dt.day(e.date) == 1 && Dt.month(e.date) == 1) {
            continue;
          }

          final stocks = e.stocks;
          var price = e.price;
          var total = stocks * price;
          var fees = None;
          if (type == Report.WITH_FEES) {
            fees = Some(Math.abs(total - e.cash));
            total = e.cash;
            price = total / stocks;
          }

          if (e.isSell) {
            final previousE = r[r.length - 1];
            final previousStocks = previousE.ts;
            final previousPrice = previousE.tp;
            final previousTotal = previousE.tt;
            final sellTotal = stocks * previousPrice;
            var finalStocks = previousStocks - stocks;
            if (finalStocks < 0) {
              finalStocks = 0;
            }
            final finalTotal = previousTotal - sellTotal;
            var finalPrice = 0.0;
            if (finalStocks > 0) {
              finalPrice = finalTotal / finalStocks;
            }

            final profits = Dec.round(total - sellTotal, 2);

            r.push(new FormRow(
              Dt.toIso(e.date),
              0, 0.0, 0.0,
              stocks, previousPrice, sellTotal,
              finalStocks, finalPrice, finalTotal,
              Some(profits), previousE.ttProfits + profits,
              fees, switch (fees) {
                  case None: previousE.ttFees;
                  case Some(v): previousE.ttFees + v;
                }
            ));
          } else {
            final previousE = r[r.length - 1];
            final previousStocks = previousE.ts;
            final previousTotal = previousE.tt;
            final finalStocks = previousStocks + stocks;
            final finalTotal = previousTotal + total;
            final finalPrice = finalTotal / finalStocks;

            r.push(new FormRow(
              Dt.toIso(e.date),
              stocks, price, total,
              0, 0.0, 0.0,
              finalStocks, finalPrice, finalTotal,
              None, previousE.ttProfits,
              fees, switch (fees) {
                  case None: previousE.ttFees;
                  case Some(v): previousE.ttFees + v;
                }
            ));
          }
        }
      }
    }
    return r;
  }

  /// Make annotation of the first of January
  ///   fn: Callback wich receive a message of error or "" if all was well.
  ///   year:
  public function setO1O1 (year: String, fn: String -> Void): Void {
    final lastYear = lastYearId();
    if (lastYear != year) {
      fn(year + " is not the last year");
      return;
    }
    final previousYear = Std.string(Std.parseInt(lastYear) - 1);
    final pdata = data[previousYear];
    if (pdata == null) {
      fn(_args(_("There is no annotation of year %0"), [previousYear]));
      return;
    }

    /// 'ucash' is cash by stock.
    final invs: Array<Map<String, {stocks: Int, price: Float, ucash: Float}>> =
      It.range(Cts.investors).map(i -> new Map()).to();

    for (a in pdata.anns) {
      final d = invs[a.inv][a.nick];
      if (a.isSell) {
        if (d == null) {
          fn(_args(_("No previous buy in annotation:\n%0"), [a.toString()]));
          return;
        }
        if (d.stocks < a.stocks) {
          fn(_args(
            _("Selling %0 stocks when there are %1\n%2"),
            [Std.string(a.stocks), Std.string(d.stocks), a.toString()]
          ));
          return;
        }
        d.stocks -= a.stocks;
        continue;
      }
      if (d == null && a.stocks > 0) {
        invs[a.inv][a.nick] = {
          stocks: a.stocks, price: a.price, ucash: a.cash / a.stocks
        };
      } if (d != null) {
        final stocks = d.stocks + a.stocks;
        d.price = (d.stocks * d.price + a.stocks * a.price) / stocks;
        d.ucash = (d.stocks * d.ucash + a.cash) / stocks;
        d.stocks = stocks;
      }
    }

    final cdata = data[lastYear];
    for (a in cdata.anns.copy()) {
      if (Dt.day(a.date) == 1 && Dt.month(a.date) == 1) {
        cdata.delete(a.id);
      }
    }

    It.range(Cts.investors).each(i -> {
      It.fromMap(invs[i])
        .filter(tp -> tp.e2.stocks > 0)
        .sort((tp1, tp2) -> Str.compare(tp1.e1, tp2.e1))
        .each(tp -> {
          final v = tp.e2;
          cdata.add(None, new Ann(
            false, Dt.mk(1, 1, Std.parseInt(lastYear)), i, tp.e1,
            v.stocks, v.price, v.ucash * v.stocks
          ));
        });
    });

    fn("");
  }

  public function toJs (): Js {
    return Js.wa(
      It.fromMap(data).map(tp -> Js.wa([Js.ws(tp.e1), tp.e2.toJs()])).to()
    );
  }

  // Static --------------------------------------------------------------------

  static function fromJs (js: Js): All {
    final a = js.ra();
    final data = new Map<String, Year>();
    for (e in a) {
      final kv = e.ra();
      data[kv[0].rs()] = Year.fromJs(kv[1]);
    }
    return new All(data);
  }

  /// Retrieve data.All from server.
  public static function request (fn: All -> Void): Void {
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("read")
    ], rp -> {
      if (rp["data"].isNull()) {
        final d = [Cts.initialYear => new Year(0, [])];
        return fn(new All(d));
      }
      final all = fromJs(rp["data"]);
      final year = all.lastYearId();
      final currentYear = Std.string(Dt.year(Date.now()));
      if (year != currentYear) {
        all.data[currentYear] = new Year(0, []);
        all.send(() -> fn(all));
        return;
      }
      fn(all);
    });
  }
}
