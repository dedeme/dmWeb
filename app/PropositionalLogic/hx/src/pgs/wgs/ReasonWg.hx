// Copyright 05-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Tp;
import dm.It;
import data.OperatorSet;
import data.Book;
import data.Reason;
import data.Step;
import I18n._;

class ReasonWg {
  final wg: Domo;
  final auxWg: Domo;
  final book: Book;
  final steps: Array<Step>;
  final mainButtons: Array<Domo>;
  final accept: (ReasonT) -> Void;
  final cancel: () -> Void;

  final buttons: Array<Domo> = [];
  final links: Array<Tp<String, Array<Domo>>>;

  public function new (
    wg: Domo, auxWg: Domo, book: Book, steps: Array<Step>,
    mainButtons: Array<Domo>,
    accept: (ReasonT) -> Void, cancel: () -> Void
  ) {
    this.wg = wg;
    this.auxWg = auxWg;
    this.book = book;
    this.steps = steps;
    this.mainButtons = mainButtons;
    this.accept = accept;
    this.cancel = cancel;

    links = [
      mkLink("sup", "┌─"),
      mkLink("the", "├─"),
      mkLink("IN", "+ " + OperatorSet.N),
      mkLink("EN", "– " + OperatorSet.N),
      mkLink("IC", "+ " + OperatorSet.C),
      mkLink("EC", "– " + OperatorSet.C),
      mkLink("IA", "+ " + OperatorSet.A),
      mkLink("EA", "– " + OperatorSet.A),
      mkLink("IK", "+ " + OperatorSet.K),
      mkLink("EK", "– " + OperatorSet.K),
      mkLink("IE", "+ " + OperatorSet.E),
      mkLink("EE", "– " + OperatorSet.E),
      mkLink("ID", "+ " + OperatorSet.D),
      mkLink("ED", "– " + OperatorSet.D),
    ];

    show();
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    for (l in links) buttons.push(Q("div").add(l.e2[0]));

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("border")
        .add(Q("tr")
          .adds(It.from(buttons).take(6).map(b -> Q("td").add(b)).to())
          .adds([Q("td"), Q("td")]))
        .add(Q("tr")
          .adds(It.from(buttons).drop(6).map(b -> Q("td").add(b)).to())))
    ;
  }

  function mkLink (id: String, tx: String): Tp<String, Array<Domo>> {
    return new Tp(
      id,
      [ Ui.link(() -> sel(id))
          .klass("link").add(Q("div").klass("frame0").html(tx)),
        Q("div").klass("frame0").html(tx),
        Q("div").klass("frame").html(tx)
      ]
    );
  }

  // Control -------------------------------------------------------------------

  function sel (operation: String): Void {
    function normalProcess (reason: ReasonT): Void {
      switch (Step.add(steps, reason, book)) {
        case Error(e):
          Ui.alert(e);
          cancel();
        case Ok(_):
          accept(reason);
      }
    }

    for (b in mainButtons) b.disabled(true);

    It.from(links).eachIx((l, i) ->
      l.e1 == operation
      ? buttons[i].removeAll().add(l.e2[2])
      : buttons[i].removeAll().add(l.e2[1])
    );

    switch (operation) {
      case "sup": new PropMaker(
        auxWg,
        prop -> accept(Reason.mkSup(prop)),
        cancel
      );
      case "the": new TheoremWg(
        auxWg,
        book,
        steps,
        (id, index, replacements) ->
          accept(Reason.mkTheorem(id, index, replacements)),
        cancel
      );
      case "IN": normalProcess(Reason.mkIN());
      case "EN": normalProcess(Reason.mkEN());
      case "IC": normalProcess(Reason.mkIC());
      case "EC": normalProcess(Reason.mkEC());
      case "IA": normalProcess(Reason.mkIA());
      case "EA": normalProcess(Reason.mkEA());
      case "IK": normalProcess(Reason.mkIK());
      case "EK": normalProcess(Reason.mkEK());
      case "IE": normalProcess(Reason.mkIE());
      case "EE": normalProcess(Reason.mkEE());
      case "ID": normalProcess(Reason.mkID());
      case "ED": normalProcess(Reason.mkED());
      default: throw(new haxe.Exception(
          "Operation '" + operation + "' not found"
        ));
    }
  }
}
