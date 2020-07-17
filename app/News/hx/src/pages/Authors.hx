// Copyright 12-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Str;
import data.Eval;
import I18n._;

/// Authors page.
class Authors {
  /// Constructor.
  public static function mk (wg: Domo): Void {
    function fmt (n: Float): String {
      return Dec.toIso(n * 100, 2);
    }

    Cts.client.send([
      "source" => Js.ws("Authors"),
      "rq" => Js.ws("read")
    ], rp -> {
      final wgs = rp["wgs"].rArray(Eval.fromJs);
      wgs.sort((e1, e2) -> Str.localeCompare(e1.id, e2.id));
      var trs: Array<Domo> = [];
      for (e in wgs) {
        trs.push(Q("tr")
          .add(Q("td")
            .klass("desc")
            .text(e.id + ": "))
          .add(Q("td")
            .klass("prer")
            .text(fmt(e.eval)))
        );
        trs.push(Q("tr")
          .add(Q("td")
            .att("colspan", "2"))
        );
      }
      wg
        .removeAll()
        .add(Q("div")
          .style("text-align:center")
          .add(Q("div")
            .klass("head")
            .html(_("Authors")))
          .add(Q("table")
            .att("align", "center")
            .att("cellspacing", 0)
            .adds(trs)))
      ;
    });
  }
}
