// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Bet data
class Bet {
  public final date: String;
  public final pay1: Float;
  public final payx: Float;
  public final pay2: Float;
  public final bet: BetBet;
  public final result: BetResult;

  public function new (
    date: String, pay1: Float, payx: Float, pay2: Float,
    bet: BetBet, result: BetResult
  ) {
    this.date = date;
    this.pay1 = pay1;
    this.payx = payx;
    this.pay2 = pay2;
    this.bet = bet;
    this.result = result;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wf(pay1),
      Js.wf(payx),
      Js.wf(pay2),
      Js.ws(switch (bet) {
        case BetB1: "1";
        case BetBx: "x";
        case BetB2: "2";
      }),
      Js.ws(switch (result) {
        case BetR0: "0";
        case BetR1: "1";
        case BetRx: "x";
        case BetR2: "2";
      }),
    ]);
  }

  public static function fromJs (js: Js): Bet {
    final a = js.ra();
    return new Bet (
      a[0].rs(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf(),
      switch (a[4].rs()) {
        case "1": BetB1;
        case "x": BetBx;
        default: BetB2;
      },
      switch (a[5].rs()) {
        case "0": BetR0;
        case "1": BetR1;
        case "x": BetRx;
        default: BetR2;
      }
    );
  }
}

enum BetBet {
  BetB1;
  BetBx;
  BetB2;
}

enum BetResult {
  BetR0;
  BetR1;
  BetRx;
  BetR2;
}
