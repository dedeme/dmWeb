// Copyright 15-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.ModalBox;
import data.MonthAnn;
import wgs.Bar;
import wgs.Form;
import I18n._;

/// Bills page.
class Bills {
  final wg: Domo;
  final anns: Array<MonthAnn>;
  final boxWg = Q("div");
  final box: ModalBox;

  function new (wg: Domo, anns: Array<MonthAnn>) {
    this.wg = wg;
    this.anns = anns;
    box = new ModalBox(boxWg, false);
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final bar = new Bar();
    wg
      .removeAll()
      .add(box.wg)
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
            .html(_("Month")))
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
              .add(Ui.link(() -> setPlace(a))
                .klass("link")
                .text(a.month.substring(4) + "/" + a.month.substring(0, 4))))
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
              .html(
                (switch (a.place) {case Some(p): p; case None: "";}) +
                "&nbsp;"
                ))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(Fns.cFmt(a.amount)))
            .add(Q("td")
              .add(new Bar(300, 2, a.amount / max).mkWg()))
          ))
    ;
  }

  // Control -------------------------------------------------------------------

  function setPlace (a: MonthAnn): Void {
    boxWg
      .removeAll()
      .add(Form.mk(
          a,
          () -> box.show(false),
          a2 -> {
            Global.client.send([
              "prg" => Js.ws("HBills"),
              "source" => Js.ws("Bills"),
              "rq" => Js.ws("setPlace"),
              "month" => Js.ws(a2.month),
              "place" => switch (a2.place){
                  case None: Js.wn();
                  case Some(p): Js.ws(p);
                }
            ], rp -> {
              mk(wg);
            });
          }
        ))
    ;
    box.show(true);
    cast(Q("#formEntry").e, js.html.InputElement).select();
    Q("#formEntry").e.focus();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("HBills"),
      "source" => Js.ws("Bills"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final anns = rp["anns"].ra().map(MonthAnn.fromJs);
      new Bills(wg, anns).show();
    });
  }
}
