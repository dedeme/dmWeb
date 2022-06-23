// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Pair (param:Int - value:Float).
class RankingEntry {
  public final param: Int;
  public final value: Float;

  public function new (param: Int, value: Float) {
    this.param = param;
    this.value = value;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(param),
      Js.wf(value)
    ]);
  }

  public static function fromJs (js: Js): RankingEntry {
    final a = js.ra();
    return new RankingEntry(
      a[0].ri(),
      a[1].rf()
    );
  }
}
