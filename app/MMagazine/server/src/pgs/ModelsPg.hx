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

/// Models page.
class ModelsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rp = new Map<String, Js>();
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
