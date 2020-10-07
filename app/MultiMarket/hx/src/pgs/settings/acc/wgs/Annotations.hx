// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.acc.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dt;
import dm.Dec;
import dm.Js;
import dm.Opt;
import data.Annotation;
import I18n._;

/// Widget to show annotations.
class Annotations {
  var wg: Domo;
  var anns: Array<Annotation>;
  var del: Option<Int -> Void>;

  /// Constructor
  ///   wg   : Container.
  ///   anns : Diary annotations.
  ///   del : Function to delete an annotation.
  public function new (
    wg: Domo, anns: Array<Annotation>, del: Option<Int -> Void>
  ) {
    this.wg = wg;
    this.anns = anns;
    this.del = del;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final fDate = d -> switch (Dt.from(d)) {
      case Some(dt): Dt.toIso(dt);
      case None: throw('${d} is not a valid date');
    }
    final fNumber = (n, d) -> Dec.toIso(n, d);

    final tdDate = d -> Q("td").klass("border").html(fDate(d));
    final tdTp = t -> Q("td").klass("border").html(t);
    final tdRest = s -> Q("td").klass("border")
      .style("text-align: left;").html(s);

    function addAnn(ann: Annotation): Array<Domo> {
      final date = tdDate(ann.date);
      final op = ann.operation;
      final se = op.se();
      if (se.ok) {
        return [date, tdTp(_("Sell")), tdRest(
          '${se.nick} | ${fNumber(se.stocks, 0)} | ${fNumber(se.price, 4)}'
        )];
      }
      final bu = op.bu();
      if (bu.ok) {
        return [date, tdTp(_("Buy")), tdRest(
          '${bu.nick} | ${fNumber(bu.stocks, 0)} | ${fNumber(bu.price, 4)}'
        )];
      }
      final st = op.st();
      if (st.ok) {
        return [date, tdTp(_("In Stock")), tdRest(
          '${st.nick} | ${fNumber(st.stocks, 0)} | ${fNumber(st.price, 4)}'
        )];
      }
      final pr = op.pr();
      if (pr.ok) {
        return [date, tdTp(_("Profits")), tdRest(
          '${fNumber(pr.amount, 2)} | ${pr.cause}'
        )];
      }
      final fe = op.fe();
      if (fe.ok) {
        return [date, tdTp(_("Fees")), tdRest(
          '${fNumber(fe.amount, 2)} | ${fe.cause}'
        )];
      }
      final pd = op.pd();
      if (pd.ok) {
        return [date, tdTp(_("Diff. +")), tdRest(
          '${fNumber(pd.amount, 2)} | ${pd.cause}'
        )];
      }
      final nd = op.nd();
      if (nd.ok) {
        return [date, tdTp(_("Diff. -")), tdRest(
          '${fNumber(nd.amount, 2)} | ${nd.cause}'
        )];
      }
      final inc = op.inc();
      if (inc.ok) {
        return [date, tdTp(_("Income")), tdRest(
          '${fNumber(inc.amount, 2)}'
        )];
      }
      final wi = op.wi();
      if (wi.ok) {
        return [date, tdTp(_("Withdrawal")), tdRest(
          '${fNumber(wi.amount, 2)}'
        )];
      }

      throw ("Unkown operation");
    }

    anns.sort((e1, e2) ->
      e1.date < e2.date ? 1
        : e1.date > e2.date ? -1
          : e2.id - e1.id
    );
    wg
      .removeAll()
      .add(Q("table")
        .adds(anns.map(ann ->
          switch (del) {
            case None:
              return Q("tr")
                .adds(addAnn(ann));
            case Some(fn):
              return Q("tr")
                .add(Ui.link(e -> fn(ann.id))
                  .add(Ui.img("delete")))
                .adds(addAnn(ann));
          })))
    ;
  }
}
