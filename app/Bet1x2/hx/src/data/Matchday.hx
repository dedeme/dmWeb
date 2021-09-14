// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Matchday data
class Matchday {
  final date: String;
  final matches: Array<Match>;

  public function new (date: String) {
    this.date = date;
    matches = [];
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(matches.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Matchday {
    final a = js.ra();
    final r = new Matchday(a[0].rs());
    for (e in a[1].ra()) {
      r.matches.push(Match.fromJs(e));
    }
    return r;
  }
}
