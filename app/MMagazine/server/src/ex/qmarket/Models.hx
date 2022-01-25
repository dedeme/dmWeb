// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.qmarket;

import dm.Opt;
import dm.Path;
import dm.File;
import dm.Js;
import cm.Cts;
import data.ModelTable;

class Models {
  static var dir = null;

  public static function init (): Void {
    dir = "/home/deme/.dmGoApp/QMarket/data/models";
  }

  public static function read(group: Int): Option<ModelTable> {
    final p = Path.cat([dir, cm.Fns.format00(group) + ".tb"]);
    if (!File.exists(p)) {
      return None;
    }
    return Some(ModelTable.fromJs(Js.from(File.read(p))));
  }

  /// Executes a function with each element of model results database.
  ///
  /// The function is run from the least parameter to the greats one in turn
  /// and it is stopped when returns 'true' or parameters are exausted.
  ///    fn: Function to execute with each record.
  ///        fn arguments are:
  ///          paramId      : Parameter to calculate values.
  ///          results      : Levels evaluation results (0...Cts.qlevels).
  ///          RETURN       : true if 'fn' must stop after execute it.
  public static function eachResult (
    fn: (Int, Array<ModelResult>) -> Bool
  ): Void {
    var stop = false;
    for (i in 0...Cts.rangesGroups) {
      final gr = Cts.rangesMin + i;
      switch (read(gr)) {
        case Some(tb):
          for (j in 0...Cts.rangesGroupElements) {
            final paramId = gr * Cts.rangesGroupElements + j;
            if (fn(paramId, tb.results[paramId])) {
              stop = true;
              break;
            }
          }
        case None: Sys.println("Model group " + gr + " not found.");
      }
      if (stop) break;
    }
  }

}
