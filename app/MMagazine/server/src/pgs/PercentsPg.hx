// Copyright 06-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import dm.It;
import dm.Dt;
import ex.qmarket.Profits;
import ex.qmarket.DiariesDb;
import db.IbexTb;
import data.DailyProfitsEntry;

/// Profits page.
class PercentsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rp = new Map<String, Js>();
        final lvs = cm.Cts.qlevels;
        final year = Dt.year(Date.now());
        final invPs : Array<Array<DailyProfitsEntry>> =
          It.range(lvs).map(inv -> Profits.read(year, inv)).to();
        final pfs = DailyProfitsEntry.mkProfits(invPs);
        rp["initialAssets"] = Js.wa(
          It.range(lvs).map(inv ->
            Js.wf(DiariesDb.read(year, inv).initialAssets())
          ).to()
        );
        rp["profits"] = Js.wa(pfs.map(e -> e.toJs()));
        rp["ibex"] = IbexTb.readJs();
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
