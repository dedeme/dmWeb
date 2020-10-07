// Copyright 12-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Market broker.
class Broker {
  /// Calculate fees of Broker + Market.
  public static function fees (amount: Float): Float {
    var broker = 9.75;
    if (amount > 25000) {
      broker = amount * 0.001;
    }

    var bolsa = 4.65 + amount * 0.00012;
    if (amount > 140000) {
      bolsa = 13.4;
    } else if (amount > 70000) {
      bolsa = 9.2 + amount * 0.00003;
    } else if (amount > 35000) {
      bolsa = 6.4 + amount * 0.00007;
    }
    bolsa += 0.11; // Execution fee.

    return broker + bolsa;
  }

  /// Returns net cost of operation.
  public static function buy (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount + Broker.fees(amount);
  }

  /// Returns net incomes of operation.
  public static function sell (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - Broker.fees(amount);
  }
}
