// Copyright 19-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Tp3;
import data.AccAnn;
import data.MonthAnn;
import wgs.Bar;
import I18n._;

/// Summary page.
class Summary {
  final wg: Domo;
  final years: Array<String>;
  final billAnns: Array<MonthAnn>;
  final stayAnns: Array<AccAnn>;

  function new (
    wg: Domo, years: Array<String>,
    billAnns: Array<MonthAnn>, stayAnns: Array<AccAnn>
  ) {
    this.wg = wg;
    this.years = years;
    this.billAnns = billAnns;
    this.stayAnns = stayAnns;
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
    final rows: Array<Tp3<String, Float, Float>> = years.map(y ->
      new Tp3(
        y,
        It.from(billAnns)
          .filter(e -> e.month.startsWith(y))
          .reduce(0.0, (r, e) -> r += e.amount),
        It.from(stayAnns)
          .filter(e -> e.date.startsWith(y))
          .reduce(0.0, (r, e) -> r += e.amount)
      )
    );

    final fmax = (n1: Float, n2) -> n1 > n2 ? n1 : n2;
    final max = It.from(rows).reduce(
      0.0, (r, e) -> return fmax(e.e2 + e.e3, r)
    );

    final sumBills = It.from(rows).reduce(0.0, (r, e) -> r + e.e2);
    final sumStays = It.from(rows).reduce(0.0, (r, e) -> r + e.e3);

    return
      Q("table")
        .att("align", "center")
        .klass("border")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(_("Year")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(_("Bills")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(_("Stays")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(_("Total")))
          .add(Q("td").klass("header")))
        .adds(rows.map(r -> Q("tr")
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
               .text(r.e1))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(Fns.cFmt(r.e2)))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(Fns.cFmt(r.e3)))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(Fns.cFmt(r.e2 + r.e3)))
            .add(Q("td")
              .add(new Bar(300, 2, (r.e2 + r.e3) / max, "#c0c080").mkWg()))
          ))
        .add(Q("tr")
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .html(_("Sums") + "&nbsp;"))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(Fns.cFmt(sumBills)))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(Fns.cFmt(sumStays)))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(Fns.cFmt(sumBills + sumStays)))
          .add(Q("td").klass("border")))
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("HBills"),
      "source" => Js.ws("Summary"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(y -> y.rs());
      final billAnns = rp["billAnns"].ra().map(MonthAnn.fromJs);
      final stayAnns = rp["stayAnns"].ra().map(AccAnn.fromJs);
      new Summary(wg, years, billAnns, stayAnns).show();
    });
  }
}
