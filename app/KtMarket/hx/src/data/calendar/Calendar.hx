// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Calendar data.
package data.calendar;

import dm.Js;

class Calendar {
  public final general: Timetable;
  public final holidays: Array<String>;
  public final specialDays: Array<MarketDay>;

  public function new (
    general: Timetable, holidays: Array<String>, specialDays: Array<MarketDay>
  ) {
    this.general = general;
    this.holidays = holidays;
    this.specialDays = specialDays;
  }

  public static function fromJs (js: Js): Calendar {
    final a = js.ra();
    return new Calendar (
      Timetable.fromJs(a[0]),
      a[1].ra().map(e -> e.rs()),
      a[2].ra().map(e -> MarketDay.fromJs(e))
    );
  }
}
