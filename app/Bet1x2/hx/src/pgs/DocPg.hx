// Copyright 26-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Tp;
import dm.Js;
import dm.It;
import I18n._;

class DocPg {
  final wg: Domo;
  final doc: Array<Tp<String, String>>;

  function new (wg: Domo, doc: Array<Tp<String, String>>) {
    doc.sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1);
    this.wg = wg;
    this.doc = doc;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .style("border-collapse : collapse;")
        .add(Q("tr")
          .add(Q("th")
            .text(_("Strategy")))
          .add(Q("th")
            .style("width:600px")
            .text(_("Documentation"))))
        .adds(doc.map(tp ->
          Q("tr")
            .add(Q("td")
              .klass("text")
              .text(tp.e1))
            .add(Q("td")
              .klass("text")
              .text(tp.e2)))))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("DocPg"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final doc = It.fromMap(rp["doc"].ro())
        .map(tp -> new Tp(tp.e1, tp.e2.rs()))
        .to()
      ;

      new DocPg(wg, doc).show();
    });
  }
}
