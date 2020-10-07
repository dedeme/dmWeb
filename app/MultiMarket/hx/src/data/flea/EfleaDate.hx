// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluate flea with date record.
class EfleaDate {
  public var date(default, null): String;
  public var eflea(default, null): Eflea;

  function new (date: String, eflea: Eflea) {
    this.date = date;
    this.eflea = eflea;
  }

  public function toJs(): Js {
    return Js.wa([
      Js.ws(date),
      eflea.toJs()
    ]);
  }

  public static function fromJs(js: Js): EfleaDate {
    final a = js.ra();
    return new EfleaDate(
      a[0].rs(),
      Eflea.fromJs(a[1])
    );
  }
}
