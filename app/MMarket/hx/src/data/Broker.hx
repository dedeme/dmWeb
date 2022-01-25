// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Market broker.
class Broker {
  /// Calculate fees.
  public static function fees (amount: Float): Float {
    amount += amount * 0.0017 + 9.53; // penalty for operating.
    final brk = (amount > 50000) ? amount * 0.001 : 9.75;
    final market = amount * 0.00003 + 0.11; // market fee + market execution
    return brk + market;
  }

  /// Returns net cost of operation.
  public static function buy (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    final tobin = amount * 0.002;
    return amount + fees(amount) + tobin;
  }

  /// Returns net incomes of operation.
  public static function sell (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - fees(amount);
  }
}
