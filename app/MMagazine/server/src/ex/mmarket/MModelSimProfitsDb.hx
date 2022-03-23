// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.mmarket;

using StringTools;

import dm.Opt;
import dm.Path;
import dm.File;
import dm.Js;
import dm.It;
import data.mmarket.SimProfitsRow;
import cm.data.ModelFloat;

/// Retrieves data from MMarket.
class MModelSimProfitsDb {
  static var dir = null;

  static function fpath (m: String): String {
    return Path.cat([dir, m + ".tb"]);
  }

  public static function init (): Void {
    dir = "/home/deme/.dmCApp/MMarket/data/simulation";
  }

  /// Returns model list.
  public static function list (): Array<String> {
    return It.from(File.dir(dir))
      .filter(e -> e.endsWith(".tb"))
      .map(f -> f.substring(0, f.length - 3))
      .to()
    ;
  }

  // Returns last ranking of simulation profits.
  //   type: It can be: 't' (total), 'c' (cash) or 'r' (refs)
  static function ranking (type: String): Array<ModelFloat> {
    function lastBest (f: String): ModelFloat {
      final model = f.substring(0, f.length - 3);
      final path = Path.cat([dir, f]);
      final a = Js.from(File.read(path)).ra()[1].ra()
        .map(e -> SimProfitsRow.fromJs(e));
      return new ModelFloat(
        model,
        Std.int(Math.round(
          It.from(a).reduce(
            0.0,
            (r, e) -> switch (type){
                case "t": e.hprofits.total > r ? e.hprofits.total : r;
                case "c": e.hprofits.cash > r ? e.hprofits.cash : r;
                default: e.hprofits.ref > r ? e.hprofits.ref : r;
              }
          )
        ))
      );
    }

    return It.from(File.dir(dir))
      .filter(e -> e.endsWith(".tb"))
      .map(f -> lastBest(f))
      .to()
    ;
  }

  /// Returns the best total assets of each model.
  public static function totalsRanking (): Array<ModelFloat> {
    return ranking("t");
  }

  /// Returns the best cash assets of each model.
  public static function cashesRanking (): Array<ModelFloat> {
    return ranking("c");
  }

  /// Returns the best reference (risk) assets of each model.
  public static function refsRanking (): Array<ModelFloat> {
    return ranking("r");
  }

}
