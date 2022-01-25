// Copyright 25-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Js;
import cm.data.ModelEval;
import cm.data.ModelEvals;
import ex.mmarket.MModelEvalsDb;

/// Evaluation models table of several dates.
class ModelEvalsTb {
  static var path: String = null;

  public static function init (parent: String): Void {
    path = Path.cat([parent, "modelEvals.tb"]);
    if (!File.exists(path)) {
      write([new ModelEvals(
        cm.Fns.lastSunday(),
        MModelEvalsDb.ranking()
      )]);
    }
  }

  public static function write (data: Array<ModelEvals>): Void {
    File.write(path, Js.wa(data.map(e -> e.toJs())).to());
  }

  public static function writeMap (data: Map<String, Array<ModelEval>>) {
    final r = [];
    for (k => v in data) r.push(new ModelEvals(k, v));
    return r;
  }

  public static function readJs (): Js {
    return Js.from(File.read(path));
  }

  public static function read (): Array<ModelEvals> {
    return Js.from(File.read(path)).ra().map(e -> ModelEvals.fromJs(e));
  }

  public static function readMap (): Map<String, Array<ModelEval>> {
    final r = new Map<String, Array<ModelEval>>();
    for (e in read()) r[e.date] = e.evals;
    return r;
  }
}
