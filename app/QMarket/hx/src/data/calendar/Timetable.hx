// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.calendar;

import dm.Js;

class Timetable {
  public final hopen: Int;
  public final mopen: Int;
  public final hclose: Int;
  public final mclose: Int;

  /// Constructor.
  ///   hopen: Open hour.
  ///   mopen: Open minute.
  ///   hclose: Close hour.
  ///   mclose: Close minute.
  public function new (hopen, mopen, hclose, mclose: Int) {
    this.hopen = hopen;
    this.mopen = mopen;
    this.hclose = hclose;
    this.mclose = mclose;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(hopen),
      Js.wi(mopen),
      Js.wi(hclose),
      Js.wi(mclose)
    ]);
  }

  public static function fromJs (js: Js): Timetable {
    var a = js.ra();
    return new Timetable(
      a[0].ri(),
      a[1].ri(),
      a[2].ri(),
      a[3].ri()
    );
  }

}
