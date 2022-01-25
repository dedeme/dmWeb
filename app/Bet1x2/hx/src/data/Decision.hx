// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import Cts;

/// Decision data.
class Decision {
  public final result: Result;
  public final points: Result;
  public final bet: Bet;
  public final decision: CtsBet_TYPE;

  function new (
    result: Result, points: Result, bet: Bet, decision: CtsBet_TYPE
  ) {
    this.result = result;
    this.points = points;
    this.bet = bet;
    this.decision = decision;
  }

  public function isOk (): Bool {
    return (result.home > result.out && decision == CtsBet_1) ||
        (result.out > result.home && decision == CtsBet_2) ||
        (result.home == result.out && decision == CtsBet_x)
      ? true
      : false
    ;
  }

  public function profits (): Float {
    return isOk()
      ? switch (decision) {
          case CtsBet_1: bet.r1;
          case CtsBet_x: bet.rx;
          case CtsBet_2: bet.r2;
        }
      : -1
    ;
  }

  public static function fromJs (js: Js): Decision {
    final a = js.ra();
    return new Decision (
      Result.fromJs(a[0]),
      Result.fromJs(a[1]),
      Bet.fromJs(a[2]),
      switch(a[3].ri()) {
        case 0: CtsBet_1;
        case 1: CtsBet_x;
        default: CtsBet_2;
      }
    );
  }
}
