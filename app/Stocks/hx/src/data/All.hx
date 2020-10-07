// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import data.FormRow;

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

  /// Returns last year data.
  public function lastYear (): Year {
    var lastYear: Year = null;
    var year = "";
    for (k => v in data) {
      if (k > year) {
        year = k;
        lastYear = v;
      }
    }
    return lastYear;
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

  /// Returns a sorted list of nicks for an investor.
  ///   type: Report.ALL, Report.WITH_FEES or number of investor.
  public function nicks (type: Int): Array<String> {
    final mp = new Map<String, String>();
    for (v in data) {
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
  public function form (type: Int, nick: String): Array<FormRow> {
    final r = [];
    for (v in data) {
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
          if (Dt.day(e.date) == 1 && Dt.month(e.date) == 1) {
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
      fn(fromJs(rp["data"]));
    });
  }
}
