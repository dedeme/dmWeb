// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Order received in Operations page.
class Order {
  public var date(default, null): String;
  public var nick(default, null): String;
  public var isSell(default, null): Bool;
  public var stocks(default, null): Int;
  public var price(default, null): Float;

  function new (
    date: String, nick: String, isSell: Bool, stocks: Int, price: Float
  ) {
    this.date = date;
    this.nick = nick;
    this.isSell = isSell;
    this.stocks = stocks;
    this.price = price;
  }

  public static function fromJs(js: Js): Order {
    final a = js.ra();
    return new Order(
      a[0].rs(),
      a[1].rs(),
      a[2].rb(),
      a[3].ri(),
      a[4].rf()
    );
  }
}
