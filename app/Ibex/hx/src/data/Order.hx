// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Market order.
class Order {
  /// Order date.
  public final date: String;
  /// "b" for buy and "s" for sell.
  public final bs: String;
  /// Stocks.
  public final stocks: Int;
  /// Stock price.
  public final price: Float;
  /// Total cost-income.
  public final value: Float;
  /// Profits if is a sale. Otherwise its value is 0.
  public final profits: Float;

  function new (
    date: String, bs: String,
    stocks: Int, price: Float, value: Float, profits: Float
  ) {
    this.date = date;
    this.bs = bs;
    this.stocks = stocks;
    this.price = price;
    this.value = value;
    this.profits = profits;
  }

  public static function fromJs (js: Js): Order {
    final a = js.ra();
    return new Order(
      a[0].rs(),
      a[1].rs(),
      a[2].ri(),
      a[3].rf(),
      a[4].rf(),
      a[5].rf()
    );
  }
}
