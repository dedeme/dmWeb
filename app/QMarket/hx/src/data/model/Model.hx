// Copyright 29-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.model;

import dm.Js;

// Model data.
class Model {
  public final qlevel: Int;
  public final id: Int;
	public final param: Float;
	public final evaluation: Eval;
	public final hevaluation: Heval;

  function new (
    qlevel: Int, id: Int, evaluation: Eval, hevaluation: Heval
  ) {
    this.qlevel = qlevel;
    this.id = id;
    this.param = id / Cts.rangesToParam;
    this.evaluation = evaluation;
    this.hevaluation = hevaluation;
  }

  public function eq (md: Model): Bool {
    return qlevel == md.qlevel && id == md.id;
  }

  public function toJs () : Js {
    return Js.wa([
      Js.wi(qlevel),
      Js.wi(id),
      evaluation.toJs(),
      hevaluation.toJs()
    ]);
  }

  public static function fromJs (js: Js) {
    final a = js.ra();
    return new Model(
      a[0].ri(),
      a[1].ri(),
      Eval.fromJs(a[2]),
      Heval.fromJs(a[3])
    );
  }

}
