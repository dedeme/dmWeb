// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import I18n._;
import I18n._args;
import data.Diary;

/// Cash of Hconta table
class Cash {
  public final entries: Array<CashEntry>;

  function new (entries: Array<CashEntry>) {
    this.entries = entries;
  }

  /// Returns 5 previous entries of ix
  public function previous (ix: Int): Array<CashEntry> {
    var start = ix - 5;
    if (start < 0) start = 0;
    return entries.slice(start, ix);
  }

  /// Returns 5 next entries of ix
  public function next (ix: Int): Array<CashEntry> {
    var end = ix + 6;
    if (end > entries.length) end = entries.length;
    return entries.slice(ix + 1, end);
  }

  public static function fromJs (js: Js): Cash {
    return new Cash(js.ra().map(e -> CashEntry.fromJs(e)));
  }
}

class CashEntry {
  public final month: String;
  public final desc: String;
  public final isIncome: Bool;
  public final am: Float;

  public function new (
    month: String, desc: String, isIncome: Bool, am: Float
  ) {
    this.month = month;
    this.desc = desc;
    this.isIncome = isIncome;
    this.am = am;
  }

  /// Returns 'true' if this == e matching Hconta and CashFlow entries.
  public function eqHcC(e: DiaryEntry): Bool {
    return e.month == month &&
      e.desc == desc &&
      e.isIncome == isIncome &&
      dm.Dec.eq(e.am, am, 0.0001)
    ;
  }

  public static function fromJs (js: Js): CashEntry {
    final a = js.ra();
    return new CashEntry(
      a[0].rs(),
      a[1].rs(),
      a[2].rb(),
      a[3].rf()
    );
  }
}
