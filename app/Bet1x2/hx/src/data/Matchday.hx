// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;
import dm.Opt;

/// Matchday data
class Matchday {
  public var number(default, null): Int;
  public final matches: Array<Match>;

  public function new (number: Int) {
    this.number = number;
    matches = [];
  }

  public function isComplete (): Bool {
    for (m in matches) {
      if (!m.bet.isComplete()) return false;
    }
    return true;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(number),
      Js.wa(matches.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Matchday {
    final a = js.ra();
    final r = new Matchday(a[0].ri());
    for (e in a[1].ra()) {
      r.matches.push(Match.fromJs(e));
    }
    return r;
  }
}
