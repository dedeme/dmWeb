// Copyright 16-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Month annotation data

package data;

import dm.Js;
import dm.Opt;

class MonthAnn {
  /// In format YYYYMM (MM between 1 and 12, both inclusive.)
  public final month: String;
  public final place: Option<String>;
  public final amount: Float;

  /// 'month' is in format YYYYMM (MM between 1 and 12, both inclusive.)
  public function new (month: String, place: Option<String>, amount: Float) {
    this.month = month;
    this.place = place;
    this.amount = amount;
  }

  public static function fromJs (js: Js): MonthAnn {
    final a = js.ra();
    return new MonthAnn(
      a[0].rs(),
      a[1].isNull() ? None : Some(a[1].rs()),
      a[2].rf()
    );
  }
}
