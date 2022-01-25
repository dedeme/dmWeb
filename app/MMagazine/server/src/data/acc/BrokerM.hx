// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

/// Model broker
/// It works equals to brokerA, but with a surcharge on fees.
class BrokerM {
  /// Returns total fees of a buy or sell operation.
  ///   amount: Operation amount.
  public static function fees (amount: Float): Float {
    final penalty = amount * 0.0017 + 9.53
    return brokerA.fees(amount) + penalty
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
