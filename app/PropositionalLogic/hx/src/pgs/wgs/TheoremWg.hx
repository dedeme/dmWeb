// Copyright 05-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Replacement;
import data.Book;
import data.Step;
import data.Prop;
import I18n._;

class TheoremWg {
  final wg: Domo;
  final book: Book;
  final steps: Array<Step>;
  final accept: (String, Int, Array<Replacement>) -> Void;
  final cancel: () -> Void;
  var thName: String;
  var bookEntry: Option<BookEntry>;
  var replacements: Option<Array<Replacement>>;

  public function new (
    wg: Domo,
    book: Book,
    steps: Array<Step>,
    accept: (String, Int, Array<Replacement>) -> Void,
    cancel: () -> Void
  ) {
    this.wg = wg;
    this.book = book;
    this.steps = steps;
    this.accept = accept;
    this.cancel = cancel;
    thName = "";
    bookEntry = None;
    replacements = None;

    show();
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    function td (){
      return  Q("td").klass("border");
    }
    function smallTd () {
      return td().style("white-space:nowrap; width:5px;");
    }
    function mkTr (
      thId: String, stepIx: Int, replacements: Array<Replacement>,
      conclusion: String
    ): Domo {
      return Q("tr")
        .add(smallTd()
          .add(Ui.link(() -> accept(thId, stepIx, replacements))
            .add(Ui.img("ok")
              .style("vertical-align:top"))))
        .add(smallTd()
          .html(_("Step") + " " + (stepIx + 1)))
        .add(td().html(conclusion))
      ;
    }

    switch (bookEntry) {
      case Some(bEntry):
      switch (replacements) {
        case Some(rpls):
          final th = Prop.replace(bEntry.demo.conclusion, rpls);
          final thStr = Prop.toString(th);
          final trs = [
            Q("tr")
              .add(td()
                .att("colspan", 3)
                .html(thStr)),
            Q("tr")
              .add(smallTd()
                .add(Ui.link(() -> {
                    bookEntry = None;
                    show();
                  })
                  .add(Ui.img("cancel")
                    .style("vertical-align:top"))))
              .add(smallTd().text(_("Cancel")))
              .add(td())
          ];

          var ix = 0;
          for (s in steps) {
            if (!s.isActive) continue;
            final pr = s.prop;
            switch (pr) {
              case C(p, q) if (Prop.eq(p, th)):
                trs.push(mkTr(
                  bEntry.id.toString(),
                  ix,
                  rpls,
                  Prop.toString(q)
                ));
              default:
            }
            switch (th) {
              case C(p, q) if (Prop.eq(p, pr)):
                trs.push(mkTr(
                  bEntry.id.toString(),
                  ix,
                  rpls,
                  Prop.toString(q)
                ));
              default:
            }
            ++ix;
          }

          if (trs.length == 1) {
            trs.unshift(
              Q("tr")
                .add(Q("td")
                  .att("colspan", 3)
                  .text(_("No theorem is applicable")))
            );
          }

          wg
            .removeAll()
            .add(Q("hr"))
            .add(Q("table")
              .att("align", "center")
              .style("border-collapse : collapse;")
              .klass("border")
              .adds(trs))
          ;
        case None:
          new ReplaceWg(
            wg, bEntry.demo.conclusion, cancelReplacement, acceptReplacement
          );
      }
      case None: // Select theorem
        replacements = None;
        final tp = book.search(thName);
        final names = tp.e1;
        names.sort(dm.Str.compare);
        final bookEntries = tp.e2;
        bookEntries.sort((tp1, tp2) -> dm.Str.compare(tp1.e1, tp2.e1));

        final btTds = names.map(n -> Q("td")
          .add(Q("button")
            .on(CLICK, () -> goNextName(n))
            .text(n))
        ).concat([
          Q("td")
            .add(Q("button")
              .style("width:50px")
              .add(Q("img")
                .style("vertical-align:bottom")
                .att("src", "img/undo.png"))
              .disabled(thName.length == 0)
              .on(CLICK, goPreviousName)),
          Q("td")
            .add(Q("button")
              .style("width:50px")
              .add(Q("img")
                .style("vertical-align:top")
                .att("src", "img/cancel.png"))
              .on(CLICK, cancel))
        ]);

        final trs = bookEntries.length == 0
          ? [ Q("tr")
                .add(Q("td")
                  .text(_("No theorem found")))
            ]
          : bookEntries.map(tp -> Q("tr")
              .add(Q("td")
                .add(Q("button")
                  .style("width:50px")
                  .add(Q("img")
                    .style("vertical-align:bottom")
                    .att("src", "img/ok.png"))
                  .on(CLICK, () -> selectTheorem(tp.e2))))
              .add(td()
                .html(tp.e1))
              .add(td()
                .html(Prop.toString(tp.e2.demo.conclusion)))
            )
        ;

        wg
          .removeAll()
          .add(Q("hr"))
          .add(Q("table")
            .att("align", "center")
            .style("border-collapse : collapse;")
            .klass("border")
            .add(Q("tr")
              .add(Q("td")
                .add(Q("table")
                  .att("align", "center")
                  .add(Q("tr")
                    .adds(btTds)))))
            .add(Q("tr")
              .add(Q("td")
                .add(Q("table")
                  .att("align", "center")
                  .style("border-collapse : collapse;")
                  .adds(trs)))))
        ;

    }
  }

  // Control -------------------------------------------------------------------

  function goNextName (n: String): Void {
    thName = n;
    show();
  }

  function goPreviousName (): Void {
    thName = thName.substring(0, thName.length - 1);
    show();
  }

  function cancelReplacement (): Void {
    bookEntry = None;
    show();
  }

  function acceptReplacement (rpls: Array<Replacement>): Void {
    replacements = Some(rpls);
    show();
  }

  function selectTheorem (bookE: BookEntry): Void {
    bookEntry = Some(bookE);
    show();
  }

}
