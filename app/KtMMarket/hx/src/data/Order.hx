// Copyright 22-Ja¡un-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Order recieved in Orders page.
class Order {
  public static final BUY = 0;
  public static final SELL = 1;
  public static final CATCH = 2;

  public var date(default, null): String;
  public var nick(default, null): String;
  public var type(default, null): Int;
  public var stocks(default, null): Int;
  public var price(default, null): Float;

  /// 'type' can be BUY, SELL or CATCH.
  function new (
    date: String, nick: String, type: Int, stocks: Int, price: Float
  ) {
    this.date = date;
    this.nick = nick;
    this.type = type;
    this.stocks = stocks;
    this.price = price;
  }

  public static function fromJs(js: Js): Order {
    final a = js.ra();
    return new Order(
      a[0].rs(),
      a[1].rs(),
      a[2].ri(),
      a[3].ri(),
      a[4].rf()
    );
  }
}
