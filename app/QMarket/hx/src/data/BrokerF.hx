// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

/// Market broker.
class BrokerF {
  /// Calculate fees of Broker + Market.
  public static function fees (amount: Float): Float {
    var penalty = amount * 0.0017 + 9.53;
    return BrokerA.fees("", amount) + penalty;
  }

  /// Returns net cost of operation.
  public static function buy (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    final tobin = amount * 0.002;
    return amount + BrokerF.fees(amount) + tobin;
  }

  /// Returns net incomes of operation.
  public static function sell (stocks: Int, price: Float): Float {
    final amount = stocks * price;
    return amount - BrokerF.fees(amount);
  }
}
