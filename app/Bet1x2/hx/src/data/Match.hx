// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;

/// Match data.
class Match {
  public final home: Club;
  public final away: Club;
  public var bet (default, null): Option<Bet>;

  public function new (home: Club, away: Club) {
    this.home = home;
    this.away = away;
    bet = None;
  }

  public function setBet (bet: Option<Bet>) {
    this.bet = bet;
  }

  public function toJs (): Js {
    return Js.wa([
      home.toJs(),
      away.toJs(),
      switch (bet) {
        case Some(b): b.toJs();
        case None: Js.wn();
      }
    ]);
  }

  public static function fromJs (js: Js): Match {
    final a = js.ra();
    final r = new Match (
      Club.fromJs(a[0]),
      Club.fromJs(a[1])
    );
    if (!a[2].isNull()) r.setBet(Some(Bet.fromJs(a[2])));
    return r;
  }
}
