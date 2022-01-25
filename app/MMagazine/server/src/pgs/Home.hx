// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import db.Rankings;

/// Home page.
class Home {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rks = Rankings.read();
        final l = rks.length;
        final rp = new Map<String, Js>();
        rp["bestRanking"] = rks[l-1].ranking[0].toJs();
        rp["isNewBestRanking"] =
          Js.wb(rks[l-1].ranking[0].param != rks[l-2].ranking[0].param);
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
