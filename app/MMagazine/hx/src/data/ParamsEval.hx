// Copyright 25-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Pair parameters - evaluation.
class ParamsEval {
  /// Parameters.
  public final params: Array<Float>;
  /// Math.round (evaluation * 100000).
  public final eval: Int;

  public function new (params: Array<Float>, eval: Int) {
    this.params = params;
    this.eval = eval;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wa(params.map(e -> Js.wf(e))),
      Js.wi(eval),
    ]);
  }

  public static function fromJs (js: Js): ParamsEval {
    final a = js.ra();
    return new ParamsEval(
      a[0].ra().map(e -> e.rf()),
      a[1].ri()
    );
  }
}
