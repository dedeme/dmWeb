// Copyright 16-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import data.AccAnn;
import wgs.Bar;
import I18n._;

/// Stays page.
class Stays {
  final wg: Domo;
  final anns: Array<AccAnn>;

  function new (wg: Domo, anns: Array<AccAnn>) {
    this.wg = wg;
    this.anns = anns;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(table()))))
    ;
  }

  function table (): Domo {
    final max = It.from(anns).reduce(
      0.0, (r, e) -> return e.amount > r ? e.amount : r
    );
    final tb = Q("table").att("align", "center").klass("border");

    return anns.length == 0
      ? tb
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center")
            .html(_("Without Data"))))
      : tb
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(_("Date")))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(_("Place")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(_("Amount")))
          .add(Q("td").klass("header")))
        .adds(anns.map(a -> Q("tr")
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
               .text(
                  a.date.substring(6) + "/" +
                  a.date.substring(4, 6) + "/" +
                  a.date.substring(0, 4))
                )
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
              .html(a.description + "&nbsp;"))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(Fns.cFmt(a.amount)))
            .add(Q("td")
              .add(new Bar(300, 2, a.amount / max, "#c080c0").mkWg()))
          ))
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("HBills"),
      "source" => Js.ws("Stays"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(AccAnn.fromJs);
      new Stays(wg, anns).show();
    });
  }
}
