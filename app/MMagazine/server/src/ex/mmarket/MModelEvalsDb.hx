// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.mmarket;

using StringTools;

import dm.Opt;
import dm.Path;
import dm.File;
import dm.Js;
import dm.It;
import cm.data.ModelEval;
import cm.data.ParamsEval;
import data.mmarket.MModelEval;

/// Retrieves data from MMarket.
class MModelEvalsDb {
  static var dir = null;

  static function fpath (m: String): String {
    return Path.cat([dir, m + ".tb"]);
  }

  public static function init (): Void {
    dir = "/home/deme/.dmCApp/MMarket/data/evals";
  }

  /// Returns model list.
  public static function list (): Array<String> {
    return It.from(File.dir(dir))
      .filter(e -> e.endsWith(".tb"))
      .map(f -> f.substring(0, f.length - 3))
      .to()
    ;
  }

  /// Returns the best result of each model.
  public static function ranking (): Array<ModelEval> {
    function lastBest (f: String): ModelEval {
      final model = f.substring(0, f.length - 3);
      final path = Path.cat([dir, f]);
      final a = Js.from(File.read(path)).ra()[1].ra()
        .map(e -> MModelEval.fromJs(e));
      return new ModelEval(
        model,
        Std.int(Math.round(
          It.from(a).reduce(0.0, (r, e) -> e.hvalue > r ? e.hvalue : r) * 100000
        ))
      );
    }

    return It.from(File.dir(dir))
      .filter(e -> e.endsWith(".tb"))
      .map(f -> lastBest(f))
      .to()
    ;
  }

  /// Returns parameters and evaluations of a model.
  ///   model: Model identifier.
  public static function paramsEvals(model: String): Array<ParamsEval>  {
    final path = fpath(model);
    return Js.from(File.read(path)).ra()[1].ra()
      .map(e -> {
        final ev = MModelEval.fromJs(e);
        return new ParamsEval(
          ev.params, Std.int(Math.round(ev.hvalue * 100000))
        );
      });
  }
}
