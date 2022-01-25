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
import data.mmarket.MModelEval;

class MModelEvalsDb {
  static var dir = null;

  public static function init (): Void {
    dir = "/home/deme/.dmCApp/MMarket/data/evals";
  }

  public static function ranking (): Array<ModelEval> {
    return It.from(File.dir(dir))
      .filter(e -> e.endsWith(".tb"))
      .map(f -> lastBest(f))
      .to()
    ;
  }

  public static function lastBest (f: String): ModelEval {
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
}
