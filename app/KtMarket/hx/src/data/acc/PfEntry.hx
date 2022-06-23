// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

import dm.Js;

// Portfolio entry
class PfEntry {
  public var nick(default, null): String;
  public var stocks(default, null): Int;
  public var price(default, null): Float;
  public var quote(default, null): Float;
  public var ref(default, null): Float;

  public function new (
    nick: String, stocks: Int, price: Float, quote: Float, ref: Float
  ) {
    this.nick = nick;
    this.stocks = stocks;
    this.price = price;
    this.quote = quote;
    this.ref = ref;
  }

  public static function fromJs (js: Js): PfEntry {
    final a = js.ra();
    return new PfEntry(
      a[0].rs(),
      a[1].ri(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf()
    );
  }
}

