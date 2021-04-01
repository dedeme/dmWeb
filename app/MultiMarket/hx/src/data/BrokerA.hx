// Copyright 12-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Market broker.
class BrokerA {
  static function isBigCia (nick: String): Bool {
    final bigCias = [
      "AENA", "AMS", "BBVA", "CABK", "CLNX", "ELE", "FER", "IBE", "ITX",
		"MTS", "REE", "REP", "SAN", "TEF"
    ];

    return bigCias.contains(nick);
  }

  /// Calculate fees of Broker + Market.
  public static function fees (nick: String, amount: Float): Float {
    var broker = 9.75;
    if (amount > 50000) {
      broker = amount * 0.001;
    }

    final isBig = (nick == "") ? false : isBigCia(nick);

    var market: Float;

    if (isBig) {
      market = amount * 0.00003;
      if (market < 1) market = 1;
    } else {
      if (amount > 140000) {
        market = 13.4;
      } else if (amount > 70000) {
        market = 9.2 + amount * 0.00003;
      } else if (amount > 35000) {
        market = 6.4 + amount * 0.00007;
      } else if (amount > 300){
        market = 4.65 + amount * 0.00012;
      } else {
        market = 1.1;
      }
    }
    market += 0.11; // Execution fee.


    return broker + market;
  }

  /// Returns net cost of operation.
  public static function buy (nick: String, stocks: Int, price: Float): Float {
    final amount = stocks * price;
    final tobin = amount * 0.002;
    return amount + BrokerA.fees(nick, amount) + tobin;
  }

  /// Returns net incomes of operation.
  public static function sell (nick: String, stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - BrokerA.fees(nick, amount);
  }
}
