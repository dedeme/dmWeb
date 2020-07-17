// Copyright 12-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Str;
import dm.Menu;
import data.Eval;
import I18n._;

/// Wrods page.
class Words {

  // View ----------------------------------------------------------------------

  static function mkBody (wg: Domo, letter: String): Void {
    function fmt (n: Float): String {
      return Dec.toIso(n * 100, 2);
    }

    Cts.client.send([
      "source" => Js.ws("Words"),
      "rq" => Js.ws("read"),
      "letter" => Js.ws(letter)
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
        .add(Q("table")
          .att("align", "center")
          .att("cellspacing", 0)
          .adds(trs))
      ;
    });
  }

  /// Constructor.
  public static function mk (wg: Domo): Void {
    final body = Q("div");
    Cts.client.send([
      "source" => Js.ws("Words"),
      "rq" => Js.ws("letters")
    ], rp -> {
      final letters = rp["letters"].rArray(e -> e.rs());
      letters.sort(Str.localeCompare);
      final lopts = [];
      var isFirst = true;
      for (e in letters) {
        if (isFirst) isFirst = false;
        else lopts.push(Menu.separator2());
        final op = Menu.toption(e, '<code>·$e·</code>', () -> mkBody(body, e));
        op.wg.setStyle("white-space", "nowrap");
        lopts.push(op);
      }
      final menu = new Menu(lopts, [], "");

      wg
        .removeAll()
        .add(Q("div")
          .style("text-align:center")
          .add(Q("div")
            .klass("head")
            .html(_("Weights")))
          .add(Q("hr"))
          .add(menu.wg)
          .add(body))
      ;
    });
  }
}
