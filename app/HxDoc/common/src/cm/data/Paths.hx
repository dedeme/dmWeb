// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Paths data
class Paths {
  public final list: Array<Lpath>;

  function new (paths: Array<Lpath>) {
    paths.sort((p1, p2) ->
      p1.lib.toUpperCase() > p2.lib.toUpperCase() ? 1 : -1
    );
    this.list = paths;
  }

  public function toJs (): Js {
    return Js.wa(list.map(p -> p.toJs()));
  }

  public static function fromJs (js: Js): Paths {
    return new Paths(js.ra().map(pjs -> Lpath.fromJs(pjs)));
  }

  /// Returns a first time initialized "Paths"
  public static function mkInitial (): Paths {
    return new Paths([]);
  }
}
