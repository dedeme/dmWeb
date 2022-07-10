// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Market broker.
class Broker {
  /// Calculate fees of Broker + Market.
  public static function fees (amount: Float): Float {
    var broker = 9.75;
    if (amount > 50000) {
      broker = amount * 0.001;
    }

    var market = amount * 0.00003;
    market += 0.11; // Execution fee.


    return broker + market;
  }

  /// Returns net cost of operation.
  public static function buy (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    final tobin = amount * 0.002;
    return amount + Broker.fees(amount) + tobin;
  }

  /// Returns net incomes of operation.
  public static function sell (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - Broker.fees(amount);
  }
}
