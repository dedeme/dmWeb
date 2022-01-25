// Copyright 06-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import dm.It;
import dm.Dt;
import ex.qmarket.Profits;
import data.DailyProfitsEntry;

/// Profits page.
class ProfitsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rp = new Map<String, Js>();
        final year = Dt.year(Date.now());
        final invPs : Array<Array<DailyProfitsEntry>> =
          It.range(cm.Cts.qlevels).map(inv -> Profits.read(year, inv)).to();
        final pfs = DailyProfitsEntry.mkProfits(invPs);
        rp["profits"] = Js.wa(pfs.map(e -> e.toJs()));
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
