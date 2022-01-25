// Copyright 06-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package ex.qmarket;

import dm.Dt;
import dm.Path;
import dm.Js;
import dm.File;
import data.DailyProfitsEntry;

class Profits {
  static var dir = null;

  public static function init (): Void {
    dir = "/home/deme/.dmGoApp/QMarket/data/acc";
  }

  /// Reads profits of an investor.
  /// If there are no data from the current year, it returns [].
  public static function read(year: Int, inv: Int): Array<DailyProfitsEntry> {
    var y = "" + year + ".tb";
    var path = Path.cat([dir, "Investor-" + inv, "profits", y]);

    if (!File.exists(path)) {
      y = "" + (year - 1) + ".tb";
      path = Path.cat([dir, "Investor-" + inv, "profits", y]);

      if (!File.exists(path)) return [];
    }

    return Js.from(File.read(path)).ra()
      .map(e -> DailyProfitsEntry.fromJs(e));
  }
}
