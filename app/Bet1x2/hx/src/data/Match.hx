// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;

/// Match data.
class Match {
  public final home: Club;
  public final away: Club;
  public final bet: Bet;

  public function new (home: Club, away: Club, ?bet) {
    this.home = home;
    this.away = away;
    this.bet = bet == null ? new Bet() : bet;
  }

  public function toJs (): Js {
    return Js.wa([
      home.toJs(),
      away.toJs(),
      bet.toJs()
    ]);
  }

  public static function fromJs (js: Js): Match {
    final a = js.ra();
    return new Match (
      Club.fromJs(a[0]),
      Club.fromJs(a[1]),
      Bet.fromJs(a[2])
    );
  }
}
