// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package update;

import ex.qmarket.Models;
import db.Rankings;
import cm.data.Ranking;
import cm.data.RankingEntry;

class RankingsUpdate {
  public static function run() {
    final lastSunday = cm.Fns.lastSunday();
    final rkData = Rankings.read();
    final day = rkData[rkData.length - 1].date;
    if (day >= lastSunday) return;

    final lastRanking = new Array<RankingEntry>();
    Models.eachResult((p, rss) -> {
      var sum = 0.0;
      for (i in 0...cm.Cts.qlevels) sum += rss[i].historicValue;
      lastRanking.push(new RankingEntry(p, sum / cm.Cts.qlevels));
      return false;
    });

    final rkGroups = new Array<RankingEntry>();
    final groupElements = Std.int(Cts.rangesGroupElements / 10); // 1000
    final midGroupElements = Std.int(groupElements / 2);
    for (i in 1...Cts.rangesGroups * 10 /* 100 */) {
      final param = lastRanking[i * groupElements].param;
      var sum = 0.0;
      for (j in 0...groupElements) {
        sum += lastRanking[i * groupElements + j - midGroupElements].value;
      }
      rkGroups.push(new RankingEntry(param, sum / groupElements));
    }

    final lastRk = new Array<RankingEntry>();
    for (i in 0...rkGroups.length - 10) {
      final param = rkGroups[i + 5].param;
      var sum = 0.0;
      for (j in 0...10) {
        sum += rkGroups[i + j].value;
      }
      lastRk.push(new RankingEntry(param, sum / 10));
    }

    lastRk.sort((e1, e2) -> e1.value < e2.value ? 1 : -1);
    lastRk.splice(Cts.rankingSize, lastRanking.length - Cts.rankingSize);

    rkData.splice(0, 1);
    rkData.push(new Ranking(lastSunday, lastRk));
    Rankings.write(rkData);
  }
}
