// Copyright 16-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Month annotation data

package data;

import dm.Js;

class MonthAnn {
  final month: String;
  final amount: Float;

  /// 'month' is between 1 and 12, both inclusive.
  public function new (month: Int, amount: Float) {
    final d = js.lib.Date.fromHaxeDate(
      new Date(2020, month - 1, 1, 12, 0, 0)
    );
    this.month = d.toLocaleDateString("es-ES", {month: cast("long")});
    this.amount = amount;
  }

  public static function fromJs (js: Js): MonthAnn {
    final a = js.ra();
    return new MonthAnn(
      a[0].ri(),
      a[1].rf()
    );
  }
}
