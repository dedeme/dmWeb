// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Hot map of a model.
class HotMap implements Dated {
  public final date: String;
  /// Sorted array.
  public final entries: Array<ParamsEval>;

  public function new (date: String, entries: Array<ParamsEval>) {
    this.date = date;
    this.entries = entries;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(entries.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): HotMap {
    final a = js.ra();
    return new HotMap(
      a[0].rs(),
      a[1].ra().map(e -> ParamsEval.fromJs(e))
    );
  }

}
