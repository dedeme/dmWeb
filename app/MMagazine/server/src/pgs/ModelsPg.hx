// Copyright 06-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import cm.data.ModelFloats;
import cm.data.ModelFloat;

/// Models page.
class ModelsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "idata":
        final type = Cgi.rqString(mrq, "type");
        final rp = new Map<String, Js>();
        rp["type"] = Js.ws(type);
        switch (type) {
          case "total":
            rp["dataGroups"] = db.ModelSimProfitsDb.readTotalsJs();
          case "cash":
            rp["dataGroups"] = db.ModelSimProfitsDb.readCashesJs();
          case "ref":
            rp["dataGroups"] = db.ModelSimProfitsDb.readRefsJs();
          default: {
            rp["type"] = Js.ws("points");
            rp["dataGroups"] = Js.wa(db.ModelEvalsTb.read()
              .map(e -> new ModelFloats(
                    e.date,
                    e.evals.map(e -> new ModelFloat(e.model, e.eval / 100))
                  ).toJs()
                )
              );
          }
        }
        return Cgi.rp(rp);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
