// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Profits entry data.
class ProfitsEntry {
  public var date(default, null): String;
  public var total(default, null): Float;
  public var acc(default, null): Float;
  public var risk(default, null): Float;

  /// Constructor.
  ///   date : Data date.
  ///   total: Total profits.
  ///   acc  : Accounting profits.
  ///   risk : Total profits with risk.
  public function new (date: String, total: Float, acc: Float, risk: Float) {
    this.date = date;
    this.total = total;
    this.acc = acc;
    this.risk = risk;
  }

  public static function fromJs (js: Js): ProfitsEntry {
    final a = js.ra();
    return new ProfitsEntry(
      a[0].rs(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf()
    );
  }
}
