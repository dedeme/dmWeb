// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;
import dm.Dt;

/// Bet data
class Bet {
  public var date: Option<Date>;
  public var pay1: Option<Float>;
  public var payx: Option<Float>;
  public var pay2: Option<Float>;
  public var bet: Option<BetResult>;
  public var result: Option<BetResult>;

  public function new () {
    date = None;
    pay1 = None;
    payx = None;
    pay2 = None;
    bet = None;
    result = None;
  }

  public function isComplete (): Bool {
    return date != None && pay1 != None && payx != None && pay2 != None &&
      bet != None && result != None;
  }

  public function fees (): Option<Float> {
    function solveMatrix (matrix: Array<Array<Float>>): Array<Float> {
      final len = matrix.length;
      if (len == 1) {
        return [-matrix[0][1] / matrix[0][0]];
      }
      final ec0 = matrix[0];
      final ec00 = ec0[0];
      final subMatrix = [];
      for (i in  1...len) {
        final ec = matrix[i];
        final m = ec[0] / ec00;
        final subEc = [];
        for (j in 1...len+1) {
          subEc.push(ec0[j] * m - ec[j]);
        }
        subMatrix.push(subEc);
      }
      final r = solveMatrix(subMatrix);
      var sum = 0.0;
      var i = 1;
      while (i < len) {
        sum += ec0[i] * r[i - 1];
        ++i;
      }
      sum += ec0[i];
      r.unshift(-sum / ec00);
      return r;
    }

    function calculate (values: Array<Float>): Float {
      final matrix = [];
      for (i in 0...values.length) {
        final a = [];
        for (j in 0...values.length) {
          a.push(j == i ? (values[i] - 1) : -1);
        }
        a.push(1);
        matrix.push(a);
      }

      final solMatrix = solveMatrix(matrix);
      var sum = 0.0;
      for (i in 0...solMatrix.length) {
        sum += solMatrix[i];
      }
      return 100 / sum;
    }

    return Opt.bind(pay1, p1 ->
      Opt.bind(payx, px ->
        Opt.bind(pay2, p2 ->
          Some(calculate([p1, px, p2]))
        )
      )
    );
  }

  public function profits (?isExpected = false): Option<Float> {
    function payValue (rs: BetResult): Option<Float> {
      return switch (rs) {
        case BetR1: Opt.bind(pay1, p -> Some((p - 1) * Cts.bet));
        case BetRx: Opt.bind(payx, p -> Some((p - 1) * Cts.bet));
        case BetR2: Opt.bind(pay2, p -> Some((p - 1) * Cts.bet));
      }
    }

    return isExpected
      ? Opt.bind(bet, rs -> payValue(rs))
      : Opt.bind(
          bet,
          betRs -> Opt.bind(
            result,
            rs -> rs == betRs ? payValue(rs) : Some(-Cts.bet)
          )
        )
    ;
  }

  public function toJs (): Js {
    return Js.wa([
      switch (date) { case None: Js.wn(); case Some(v): Js.ws(Dt.to(v)); },
      switch (pay1) { case None: Js.wn(); case Some(v): Js.wf(v); },
      switch (payx) { case None: Js.wn(); case Some(v): Js.wf(v); },
      switch (pay2) { case None: Js.wn(); case Some(v): Js.wf(v); },
      switch (bet) {
        case None: Js.wn(); case Some(v): Js.ws(resultToStr(v));
      },
      switch (result) {
        case None: Js.wn(); case Some(v): Js.ws(resultToStr(v));
      }
    ]);
  }

  public static function fromJs (js: Js): Bet {
    final a = js.ra();
    final r = new Bet();
    if (!a[0].isNull()) r.date = Some(Opt.get(Dt.from(a[0].rs())));
    if (!a[1].isNull()) r.pay1 = Some(a[1].rf());
    if (!a[2].isNull()) r.payx = Some(a[2].rf());
    if (!a[3].isNull()) r.pay2 = Some(a[3].rf());
    if (!a[4].isNull()) r.bet = Some(resultFromStr(a[4].rs()));
    if (!a[5].isNull()) r.result = Some(resultFromStr(a[5].rs()));
    return r;
  }

  public static function resultFromStr (r: String): BetResult {
    return switch (r) {
      case "1": BetR1;
      case "x": BetRx;
      default: BetR2;
    }
  }

  public static function resultToStr (r: BetResult): String {
    return switch (r) {
      case BetR1: "1";
      case BetRx: "x";
      case BetR2: "2";
    }
  }

}

enum BetResult {
  BetR1;
  BetRx;
  BetR2;
}
