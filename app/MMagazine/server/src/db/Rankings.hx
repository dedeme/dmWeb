// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Js;
import cm.data.Ranking;
import cm.data.RankingEntry;

/// Table with rankings of parameters evaluation.
class Rankings {
  static var path: String = null;

  public static function init (parent: String): Void {
    path = Path.cat([parent, "rankings.tb"]);
    if (!File.exists(path)) {
      final data = new Array<Ranking>();
      for (ri in 0...Cts.totalRankings /* 4 */) {
        final row = new Array<RankingEntry>();
        for (ei in 0...Cts.rankingSize /* 40 */) {
          row.push(new RankingEntry(0, 0));
        }
        data.push(new Ranking("20000101", row));
      }
      write(data);
    }
  }

  public static function write (data: Array<Ranking>): Void {
    File.write(
      path,
      Js.wa(data.map(r -> r.toJs())).to()
    );
  }

  public static function readJs (): Js {
    return Js.from(File.read(path));
  }

  public static function read (): Array<Ranking> {
    return readJs().ra().map(r -> Ranking.fromJs(r));
  }
}
