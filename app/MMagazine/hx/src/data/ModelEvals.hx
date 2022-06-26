// Copyright 25-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Unsorted model evaluations of a date.
class ModelEvals implements Dated {
  public final date: String;
  /// Unsorted array.
  public final evals: Array<ModelEval>;

  public function new (date: String, evals: Array<ModelEval>) {
    this.date = date;
    this.evals = evals;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(evals.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): ModelEvals {
    final a = js.ra();
    return new ModelEvals(
      a[0].rs(),
      a[1].ra().map(e -> ModelEval.fromJs(e))
    );
  }
}
