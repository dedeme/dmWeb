// Copyright 04-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Ranking data of a day.
class Ranking {
  public final date: String;
  public final ranking: Array<RankingEntry>;

  public function new (date: String, ranking: Array<RankingEntry>) {
    this.date = date;
    this.ranking = ranking;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(ranking.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Ranking {
    final a = js.ra();
    return new Ranking(
      a[0].rs(),
      a[1].ra().map(e -> RankingEntry.fromJs(e))
    );
  }
}
