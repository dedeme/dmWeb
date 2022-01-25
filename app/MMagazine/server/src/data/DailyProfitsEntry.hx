// Copyright 06-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dt;
import dm.It;
import cm.data.ProfitsEntry;

class DailyProfitsEntry {
  public final date: String;
  public final profits: Float;
  public final cash: Float;
  public final risk: Float;

  public function new (
    date: String, profits: Float, cash: Float, risk: Float
  ) {
    this.date = date;
    this.profits = profits;
    this.cash = cash;
    this.risk = risk;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wf(profits),
      Js.wf(cash),
      Js.wf(risk)
    ]);
  }

  public static function fromJs (js: Js): DailyProfitsEntry {
    final a = js.ra();
    return new DailyProfitsEntry(
      a[0].rs(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf()
    );
  }

  /// Calculate weekly profits of investors.
  ///   invPs: Daily profits of investors.
  public static function mkProfits (
    invPs: Array<Array<DailyProfitsEntry>>
  ): Array<ProfitsEntry> {
    final r = [];

    var now = Date.now();
    var date = Dt.mk(1, 1, Dt.year(now));
    var ixs: Array<Int> = It.range(cm.Cts.qlevels).map(i -> 0).to();
    while (Dt.df(date, now) <= 0) {
      if (Dt.weekDay(date) == 0) {
        final day = Dt.to(date);
        final pfs: Array<Float> = [];
        for (inv in 0...cm.Cts.qlevels) {
          var ix = ixs[inv];
          final invp: Array<DailyProfitsEntry> = invPs[inv];
          while (ix < invp.length && invp[ix].date <= day) {
            ++ix;
          }
          if (ix > 0) {
            ixs[inv] = ix;
            pfs.push(invp[ix - 1].profits);
          } else {
            if (invp.length > 0) {
              pfs.push(invp[0].profits);
            } else {
              pfs.push(1.0);
            }
          }
        }
        r.push(new ProfitsEntry(day, pfs));
      }

      date = Dt.add(date, 1);
    }

    return r;
  }
}
