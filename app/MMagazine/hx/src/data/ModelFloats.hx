// Copyright 25-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Unsorted model evaluations of a date.
class ModelFloats implements Dated {
  public final date: String;
  /// Unsorted array.
  public final values: Array<ModelFloat>;

  public function new (date: String, values: Array<ModelFloat>) {
    this.date = date;
    this.values = values;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(values.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): ModelFloats {
    final a = js.ra();
    return new ModelFloats(
      a[0].rs(),
      a[1].ra().map(e -> ModelFloat.fromJs(e))
    );
  }
}
