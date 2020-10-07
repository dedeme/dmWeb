// Copyright 12-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.Dec;
import data.Cts;
import data.Broker;
import I18n._;

/// TextArea to show operations.
class Operations {
  var dates: Array<String>;
  var opens: Array<Float>;
  var closes: Array<Float>;
  var refs: Array<Float>;
  public final wg = Q("TextArea")
    .att("spellcheck", false)
    .att("cols", 78)
    .att("rows", 20)
  ;

  /// Constructor.
  ///   dates : Operations dates.
  ///   opens : Operations opens.
  ///   closes: Operations closess.
  ///   refs  : Operations references.
  public function new (
    dates:Array<String>, opens: Array<Float>,
    closes: Array<Float>, refs: Array<Float>
  ) {
    this.dates = dates;
    this.opens = opens;
    this.closes = closes;
    this.refs = refs;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final ds = dates;
    final os = opens;
    final cs = closes;
    final rs = refs;
    final bet: Float = Cts.bet;
    var stocks = 0;
    var cash = bet;
    var toSell = true;
    var toDo = false;

    final rule = "|" + fff(CV) + ":" + fff(DATE) + ":" + fff(STOCKS) + ":" +
      fff(PRICE) + ":" + fff(BALANCE) + ":" + fff(PROFITS) + ":" +
      fff(RATIO) + "|\n";

    var tx = rule;
    tx += "|" + fc("*", CV) + ":" + fc(_("Date"), DATE) + ":" +
      fc(_("Stocks(Operations)"), STOCKS) + ":" + fc(_("Price"), PRICE) + ":" +
      fc(_("Balance(Operations)"), BALANCE) + ":" + fc(_("Profits"), PROFITS) +
      ":" + fc(_("Ratio"), RATIO) + "|\n";
    tx += rule;

    for (i in 0...ds.length) {
      final oq = os[i];
      if (toDo && oq > 0) {
        if (toSell) { // there is buy order.
          stocks = Std.int(Cts.bet / oq);
          cash -= Broker.buy(stocks, oq);

          final date = ds[i];
          tx += row(
            _("B(uy)"), date, stocks, oq,
            None, None, None
          );
        } else if (stocks > 0) {
          cash += Broker.sell(stocks, oq);

          final date = ds[i];
          final profits = cash - bet;
          tx += row(
            _("S(ell)"), date, stocks, oq,
            Some(cash),
            Some(profits),
            Some(profits / bet)
          );

          stocks = 0;
        }
        toDo = false;
      }

      final q = cs[i];

      if (q < 0) continue;

      final ref = rs[i];
      if (toSell) {
        if (ref > q) {
          toDo = true;
          toSell = false;
        }
      } else if (ref < q) {
        toDo = true;
        toSell = true;
      }
    }

    if (stocks > 0) {
      final q = lastClose(cs);
      cash += Broker.sell(stocks, q);
      final date = ds[ds.length - 1];
      final profits = cash - bet;
      tx += row(
        "V", date, stocks, q,
        Some(cash),
        Some(profits),
        Some(profits / bet)
      );
    }

    tx += rule;

    wg.text(tx);
  }

  // Static --------------------------------------------------------------------

  static final CV = 3;
  static final DATE = 12;
  static final STOCKS = 9;
  static final PRICE = 10;
  static final BALANCE = 13;
  static final PROFITS = 12;
  static final RATIO = 10;

  static function lastClose (qs: Array<Float>): Float {
    var ix = qs.length - 1;
    while (ix >= 0) {
      if (qs[ix] > 0) return qs[ix];
      --ix;
    }
    return 1;
  }

  // Format filled.
  static function ff (width: Int): String {
    var r = "";
    while (width-- > 0) r += " ";
    return r;
  }

  // Format filled with hyphens.
  static function fff (width: Int): String {
    var r = "";
    while (width-- > 0) r += "-";
    return r;
  }

  static function fr (tx, width): String {
    final df = width - tx.length;
    return ff(df) + tx;
  }

  static function fc (tx: String, width: Int): String {
    final df = width - tx.length;
    final ldf = Std.int(df / 2);
    var rdf = df - ldf;
    var r = ff(ldf) + tx;
    while (rdf-- > 0) r += " ";
    return r;
  }

  static function row (
    op: String, date: String, stocks: Int, price: Float,
    balance: Option<Float>, profits: Option<Float>, ratio: Option<Float>
  ) {
    final balanceN = Opt.get(balance);
    final bal = balanceN != null ? Dec.toIso(balanceN, 2) : "";
    final profitsN = Opt.get(profits);
    final prof = profitsN != null ? Dec.toIso(profitsN, 2) : "";
    final ratioN = Opt.get(ratio);
    final rat = ratioN != null ? Dec.toIso(ratioN, 4) : "";

    return "|" +
      fc(op, CV) + ":" +
      fc(date, DATE) + ":" +
      fr(Dec.toIso(stocks, 0), STOCKS) + ":" +
      fr(Dec.toIso(price, 4), PRICE) + ":" +
      fr(bal, BALANCE) + ":" +
      fr(prof, PROFITS) + ":" +
      fr(rat, RATIO) + "|\n"
    ;
  }
}
