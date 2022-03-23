// Copyright 27-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;

/// Models page.
class HotMapsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final rp = new Map<String, Js>();
        var model = Cgi.rqString(mrq, "model");
        final mds = ex.mmarket.MModelEvalsDb.list();
        model = mds.contains(model) ? model : mds[0];
        rp["models"] = Js.wa(mds.map(e -> Js.ws(e)));
        rp["model"] = Js.ws(model);
        rp["mapsGroup"] = db.HotMapsDb.readJs(model);
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
