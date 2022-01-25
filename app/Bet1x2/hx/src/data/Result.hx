// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Result data.
class Result {
  public final home: Int;
  public final out: Int;

  function new (home: Int, out: Int) {
    this.home = home;
    this.out = out;
  }

  public function homePoints (): Int {
    return home == out ? 1 : home > out ? 2 : 0;
  }

  public function outPoints (): Int {
    return home == out ? 1 : out > home ? 2 : 0;
  }

  public static function fromJs (js: Js): Result {
    final a = js.ra();
    return new Result (
      a[0].ri(),
      a[1].ri()
    );
  }
}
