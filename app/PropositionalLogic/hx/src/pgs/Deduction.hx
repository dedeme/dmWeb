// Copyright 31-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Opt;
import dm.Dec;
import pgs.wgs.DemoWg;
import pgs.wgs.ReasonWg;
import data.Book;
import data.Reason;
import data.Step;
import data.Demo;
import I18n._;
import I18n._args;

/// Deduction page.
class Deduction {
  final wg: Domo;
  final book: Book;
  final reasons: Array<ReasonT>;
  final redo: Array<ReasonT>;

  final actionWg = Q("div");
  final reasonWg = Q("div");

  function new (
    wg: Domo, book: Book, reasons: Array<ReasonT>, redo: Array<ReasonT>
  ) {
    this.wg = wg;
    this.book = book;
    this.reasons = reasons;
    this.redo = redo;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    var steps: Array<Step> = [];
    for (r in reasons) {
      switch (Step.add(steps, r, book)) {
        case Error(e):
          steps = [];
          Ui.alert(e);
          break;
        case Ok(ss): steps = ss;
      }
    }
    final demoWg = Q("div");
    final auxWg = Q("div");

    new DemoWg(demoWg, book, steps);
    final mainButtons = mkActionWg(actionWg);
    new ReasonWg(reasonWg, auxWg, book, steps, mainButtons, add, show);

    wg
      .removeAll()
      // TESTS
      //.add(Q("div")
      //  .add(Q("button")
      //    .text("Tests")
      //    .on(CLICK, test.AllTests.run)))
      // END TESTS
      .add(demoWg)
      .add(actionWg)
      .add(reasonWg)
      .add(auxWg)
    ;
  }

  // Returns main buttons.
  function mkActionWg(wg: Domo): Array<Domo> {
    final b1 = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:bottom")
        .att("src", "img/undo.png"))
        .on(CLICK, undo)
    ;
    final b2 = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:bottom")
        .att("src", "img/redo.png"))
        .on(CLICK, redoAction)
    ;
    final b3 = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:top")
        .att("src", "img/cancel.png"))
      .on(CLICK, reset)
    ;
    final b4 = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:top")
        .att("src", "img/save.png"))
      .on(CLICK, save)
    ;

    if (reasons.length == 0) b1.disabled(true);
    if (redo.length == 0) b2.disabled(true);
    if (reasons.length == 0 && redo.length == 0) b3.disabled(true);
    final level = Reason.level(reasons);
    if (reasons.length == 0 || level != 0) b4.disabled(true);

    wg
      .removeAll()
      .add(Cts.wait.wg)
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(b1))
          .add(Q("td")
            .add(b2))
          .add(Q("td")
            .add(b3))
          .add(Q("td")
            .add(b4))))
    ;

    return [b1, b2, b3, b4];
  }

  // Dialog for modify theorem.
  function mkActionWg2 (wg: Domo, bookId: String): Void {
    final demo = Opt.eget(book.get(bookId));
    var steps: Array<Step> = [];
    for (r in demo.reasons) {
      switch (Step.add(steps, r, book)) {
        case Error(e): throw(new haxe.Exception(e));
        case Ok(ss): steps = ss;
      }
    }
    final dmWg = Q("div");
    new DemoWg(dmWg, book, steps);
    wg
      .removeAll()
      .add(Cts.wait.wg)
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .html(_("Theorem is duplicated:"))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .add(dmWg)))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .html(_("Replace?"))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("button")
                    .on(CLICK, show)
                    .text(_("Cancel"))))
                .add(Q("td")
                  .add(Q("button")
                    .on(CLICK, () -> modify(bookId))
                    .text(_("Accept")))))))))
    ;
  }

  // Control -------------------------------------------------------------------

  function serverUpdate (): Void {
    Cts.client.send([
      "source" => Js.ws("Deduction"),
      "rq" => Js.ws("update"),
      "reasons" => Js.wa(reasons.map(Reason.toJs)),
      "redo" => Js.wa(redo.map(Reason.toJs)),
    ], rp -> {
    });
  }

  function add(reason: ReasonT): Void {
    reasons.push(reason);
    redo.splice(0, redo.length);
    show();
    serverUpdate();
  }

  function undo(): Void {
    redo.push(reasons.pop());
    show();
    serverUpdate();
  }

  function redoAction(): Void {
    reasons.push(redo.pop());
    show();
    serverUpdate();
  }

  function reset(): Void {
    if (!Ui.confirm(_("Reset the current demonstration?"))) return;
    reasons.splice(0, reasons.length);
    redo.splice(0, redo.length);
    show();
    serverUpdate();
  }

  function save(): Void {
    var steps: Array<Step> = [];
    for (r in reasons) {
      switch (Step.add(steps, r, book)) {
        case Error(e):
          steps = [];
          Ui.alert(e);
          return;
        case Ok(ss): steps = ss;
      }
    }
    if (!Ui.confirm(_("Save demonstration?"))) return;
    Cts.wait.show(true);
    final demo = new Demo(reasons, steps[steps.length - 1].prop);
    switch (book.add(demo)) {
      case Ok(id):
        Cts.client.send([
          "source" => Js.ws("Deduction"),
          "rq" => Js.ws("save"),
          "book" => book.toJs()
        ], rp -> {
          Cts.wait.show(false);
          Ui.alert(_args(_("Demostration saved with id '%0'"), [id]));
          mk(wg);
        });
      case Error(id):
        Cts.wait.show(false);
        if (demo.dependsOnTheorem(id)) {
          Ui.alert(_("Theorem not saved because it depends on itself"));
          return;
        }
        mkActionWg2(actionWg, id);
        reasonWg.removeAll();
    }
  }

  function modify(bookId: String): Void {
    var steps: Array<Step> = [];
    for (r in reasons) {
      switch (Step.add(steps, r, book)) {
        case Error(e): throw(new haxe.Exception(e));
        case Ok(ss): steps = ss;
      }
    }
    Cts.wait.show(true);
    final demo = new Demo(reasons, steps[steps.length - 1].prop);
    book.replace(bookId, demo);
    Cts.client.send([
      "source" => Js.ws("Deduction"),
      "rq" => Js.ws("save"),
      "book" => book.toJs()
    ], rp -> {
      Cts.wait.show(false);
      Ui.alert(_args(_("Demostration saved with id '%0'"), [bookId]));
      mk(wg);
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("Deduction"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final book = Book.fromJs(rp["book"]);
      final reasons = rp["reasons"].ra().map(Reason.fromJs);
      final redo = rp["redo"].ra().map(Reason.fromJs);
      new Deduction(wg, book, reasons, redo).show();
    });
  }
}
