// Copyright 25-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Pair model - evaluation.
class ModelEval {
  /// Model id.
  public final model: String;
  /// Math.round (evaluation * 100000).
  public final eval: Int;

  public function new (model: String, eval: Int) {
    this.model = model;
    this.eval = eval;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(model),
      Js.wi(eval),
    ]);
  }

  public static function fromJs (js: Js): ModelEval {
    final a = js.ra();
    return new ModelEval(
      a[0].rs(),
      a[1].ri()
    );
  }
}
