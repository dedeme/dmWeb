// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Evaluate flea with date record.
class Frank {
  public var date(default, null): String;
  public var efleas(default, null): Array<Eflea>;

  function new (date: String, efleas: Array<Eflea>) {
    this.date = date;
    this.efleas = efleas;
  }

  public function toJs(): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(efleas.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs(js: Js): Frank {
    final a = js.ra();
    return new Frank(
      a[0].rs(),
      a[1].ra().map(e -> Eflea.fromJs(e))
    );
  }
}
