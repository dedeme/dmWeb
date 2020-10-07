// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import I18n._;

/// Annotation data.
class Ann {
  /// Identifier
  public var id: Int;
  /// One of Ann.E(XISTENCES), Ann.B(UY), Ann.S(ELL)
  public var isSell(default, null): Bool;
  /// Date of annotation.
  public var date(default, null): Date;
  /// Identifier of invester (0, 1, 2...)
  public var inv(default, null): Int;
  /// Company nick
  public var nick(default, null): String;
  /// Annotation stocks. Its value must be > 0.
  public var stocks(default, null): Int;
  /// Price of each stock. Its value must be >= 0.
  public var price(default, null): Float;
  /// Total value of operation. Its value must be >= 0 and include fees.
  public var cash(default, null): Float;

  public function new (
    isSell: Bool, date: Date, inv: Int,
    nick: String, stocks: Int, price: Float, cash: Float
  ) {
    this.isSell = isSell;
    this.date = date;
    this.inv = inv;
    this.nick = nick;
    this.stocks = stocks;
    this.price = price;
    this.cash = cash;
  }

  public function update(a: Ann): Void {
    isSell = a.isSell;
    date = a.date;
    inv = a.inv;
    nick = a.nick;
    stocks = a.stocks;
    price = a.price;
    cash = a.cash;
  }

  public function toString(): String {
    return "| " + (isSell ? _("S") : _("B")) + " | " +
      Dt.toIso(date) + " | " +
      inv + " | " +
      nick + " | " +
      Dec.toIso(stocks, 0) + " | " +
      Dec.toIso(price, 4) + " | " +
      Dec.toIso(cash, 2) + " |"
    ;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(id),
      Js.wb(isSell),
      Js.ws(Dt.to(date)),
      Js.wi(inv),
      Js.ws(nick),
      Js.wi(stocks),
      Js.wf(price),
      Js.wf(cash)
    ]);
  }

  // Static --------------------------------------------------------------------

  public static function fromJs (js: Js): Ann {
    final a = js.ra();
    final ann = new Ann(
      a[1].rb(),
      Opt.eget(Dt.from(a[2].rs())),
      a[3].ri(),
      a[4].rs(),
      a[5].ri(),
      a[6].rf(),
      a[7].rf()
    );
    ann.id = a[0].ri();
    return ann;
  }
}
