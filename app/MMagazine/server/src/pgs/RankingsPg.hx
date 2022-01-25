// Copyright 04-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import db.Rankings;

/// Rankings page.
class RankingsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rp = new Map<String, Js>();
        rp["rankings"] = Rankings.readJs();
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
