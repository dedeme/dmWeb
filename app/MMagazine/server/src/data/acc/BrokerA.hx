// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

/// Accounting broker.
class BrokerA {
  /// Returns total fees of a buy or sell operation.
  ///   amount: Operation amount.
  public static function fees (amount: Float): Float {
    final brk = amount > 50000 ? amount * 0.001 : 9.75;
    final market = amount * 0.00003 + 0.11;
    return brk + market;
  }

  /// Returns net cost of operation (cost + fees).
  ///   stocks: Stocks number.
  ///   price  ; Stocks price.
  public static function buy (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    final tobin = amount * 0.002;
    return amount + fees(amount) + tobin;
  }

  /// Returns net incomes of operation (incomes - fees).
  ///   stocks: Stocks number.
  ///   price : Stocks price.
  public static function sell (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - fees(amount);
  }
}
