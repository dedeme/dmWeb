// Copyright 28-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Pair Model - Float.
class ModelFloat {
  /// Model id.
  public final model: String;
  public final value: Float;

  public function new (model: String, value: Float) {
    this.model = model;
    this.value = value;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(model),
      Js.wf(value),
    ]);
  }

  public static function fromJs (js: Js): ModelFloat {
    final a = js.ra();
    return new ModelFloat(
      a[0].rs(),
      a[1].rf()
    );
  }
}
