// Copyright 12-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.It;
import data.Order;
import data.Broker;
import I18n._;

/// Widgets to show market orders.
class OrdersWg {
  var orders: Array<Order>;
  var nicks: Array<String>;
  var lastCloses: Array<Float>;

  public final wg = Q("TextArea")
    .att("spellcheck", false)
    .att("cols", 68)
    .att("rows", 20)
  ;

  /// Constructor
  ///   orders  : Orders to show.
  ///   nicks   : Nick names list.
  ///   lastCloses: Last closes of each nick.
  public function new (
    orders: Array<Order>, nicks: Array<String>, lastCloses: Array<Float>
  ) {
    this.orders = orders;
    this.nicks = nicks;
    this.lastCloses = lastCloses;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    var cash: Float = Cts.initialCapital;

    final rule = "|" + fff(NUM) + ":" + fff(CV) + ":" + fff(DATE) + ":" +
      fff(NICK) + ":" + fff(STOCKS) + ":" + fff(PRICE) + ":" + fff(CASH) +
      "|\n";

    var tx = rule;
    tx += "|" + fc(_("Nº"), NUM) + ":" + fc("*", CV) + ":" +
      fc(_("Date"), DATE) + ":" + fc(_("Nick"), NICK) + ":" +
      fc(_("Stocks(Operations)"), STOCKS) + ":" + fc(_("Price"), PRICE) + ":" +
      fc(_("Cash"), CASH) + "|\n";
    tx += rule;
    final pf = nicks.map(e -> 0);
    It.from(orders).eachIx((o, ix) -> {
      final op = o.isSell ? _("S(ell)") : _("B(uy)");
      final date = o.date;
      final nick = o.nick;
      final stocks = o.stocks;
      final price = o.price;
      final nickIx = nicks.indexOf(nick);
      if (o.isSell) {
        cash += Broker.sell(stocks, price);
        pf[nickIx] = 0;
      } else {
        cash -= Broker.buy(stocks, price);
        pf[nickIx] = stocks;
      }
      tx += row(ix + 1, op, date, nick, stocks, price, cash);
    });
    It.from(pf).eachIx((e, i) -> {
      if (pf[i] > 0)
        cash += Broker.sell(pf[i], lastCloses[i]);
    });
    tx += "|" + ff(NUM) + ":" + ff(CV) + ":" + ff(DATE) + ":" +
      ff(NICK) + ":" + ff(STOCKS) + ":" + ff(PRICE) + ":" +
      fr(Dec.toIso(cash, 2), CASH) + "|\n";
    tx += rule;

    wg.text(tx);
  }


  // Static --------------------------------------------------------------------

  static final NUM = 5;
  static final CV = 3;
  static final DATE = 12;
  static final NICK = 6;
  static final STOCKS = 9;
  static final PRICE = 10;
  static final CASH = 13;

  // Format filled.
  static function ff (width) {
    var r = "";
    while (width-- > 0) r += " ";
    return r;
  }

  // Format filled with hyphens.
  static function fff (width) {
    var r = "";
    while (width-- > 0) r += "-";
    return r;
  }

  static function fr (tx, width) {
    final df = width - tx.length;
    return ff(df) + tx;
  }

  static function fc (tx, width) {
    final df = width - tx.length;
    final ldf = Std.int(df / 2);
    var rdf = df - ldf;
    var r = ff(ldf) + tx;
    while (rdf-- > 0) r += " ";
    return r;
  }

  static function row (num, op, date, nick, stocks, price, cash) {
    return "|" +
      fr(Dec.toIso(num, 0), NUM) + ":" +
      fc(op, CV) + ":" +
      fc(date, DATE) + ":" +
      fc(nick, NICK) + ":" +
      fr(Dec.toIso(stocks, 0), STOCKS) + ":" +
      fr(Dec.toIso(price, 4), PRICE) + ":" +
      fr(Dec.toIso(cash, 2), CASH) + "|\n"
    ;
  }

}
