// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.qmarket;

import dm.Path;
import dm.Opt;
import dm.Js;
import dm.File;
import data.acc.Diary;

/// Retrieves diaries of investors from QMarket.
class DiariesDb {
  static var dir = null;

  public static function init (): Void {
    dir = "/home/deme/.dmGoApp/QMarket/data/acc";
  }

  public static function read(year: Int, inv: Int): Diary {
    var p = Path.cat([dir, "Investor-" + inv, "diaries", year + ".tb"]);
    if (!File.exists(p)) {
      final year2 = year - 1;
      final p2 = Path.cat([dir, "Investor-" + inv, "diaries", year + ".tb"]);

      if (!File.exists(p2))
        throw new haxe.Exception('Paths not found:\n${p}\n${p2}');

      p = p2;
    }
    return Diary.fromJs(Js.from(File.read(p)));
  }
}
