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
import data.Prop;
import data.Reason;
import data.Step;
import I18n._;
import I18n._args;

/// Theorems page.
class TheoremsPg {
  final wg: Domo;
  final book: Book;
  final thName: String;

  final demoDiv = Q("div");

  function new (
    wg: Domo, book: Book, thName: String
  ) {
    this.wg = wg;
    this.book = book;
    this.thName = thName;

    show();
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    function td (){
      return  Q("td").klass("border");
    }
    function smallTd () {
      return td().style("white-space:nowrap; width:5px;");
    }


    final tp = book.search(thName);
    final names = tp.e1;
    names.sort(dm.Str.compare);
    final bookEntries = tp.e2;
    bookEntries.sort((tp1, tp2) -> dm.Str.compare(tp1.e1, tp2.e1));

    final btTds = names.map(n -> Q("td")
      .add(Q("button")
        .on(CLICK, () -> thNameUpdate(n))
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
          .disabled(thName.length == 0)
          .on(CLICK, () -> thNameUpdate("")))
    ]);

    final trs = bookEntries.length == 0
      ? [ Q("tr")
            .add(Q("td")
              .text(_("No theorem found")))
        ]
      : It.range(bookEntries.length).map(i -> {
          final tp = bookEntries[i];
          return Q("tr")
            .add(Q("td")
              .add(Q("button")
                .style("width:50px")
                .add(Q("img")
                  .style("vertical-align:bottom")
                  .att("src", "img/trash.png"))
                .on(CLICK, () -> remove(tp.e1))))
            .add(Q("td")
              .add(Q("button")
                .style("width:50px")
                .add(Q("img")
                  .style("vertical-align:bottom")
                  .att("src", "img/go-up.png"))
                .disabled(i == 0)
                .on(CLICK, () ->
                  interchange(tp.e2.id, bookEntries[i - 1].e2.id))))
            .add(Q("td")
              .add(Q("button")
                .style("width:50px")
                .add(Q("img")
                  .style("vertical-align:bottom")
                  .att("src", "img/go-down.png"))
                .disabled(i == bookEntries.length - 1)
                .on(CLICK, () ->
                  interchange(tp.e2.id, bookEntries[i + 1].e2.id))))
            .add(Q("td")
              .add(Q("button")
                .style("width:50px")
                .add(Q("img")
                  .style("vertical-align:bottom")
                  .att("src", "img/demo-show.png"))
                .on(CLICK, () -> showDemo(tp.e2.demo.reasons))))
            .add(td()
              .html(tp.e1))
            .add(td()
              .html(Prop.toString(tp.e2.demo.conclusion)))
          ;
        }).to()
    ;

    demoDiv.removeAll();

    wg
      .removeAll()
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
      .add(demoDiv)
    ;

  }

  // Control -------------------------------------------------------------------

  function thNameUpdate (n: String): Void {
    Cts.client.send([
      "source" => Js.ws("Theorems"),
      "rq" => Js.ws("update"),
      "name" => Js.ws(n)
    ], rp -> {
      mk(wg);
    });
  }

  function goPreviousName (): Void {
    thNameUpdate(thName.substring(0, thName.length - 1));
  }

  function showDemo (reasons: Array<ReasonT>): Void {
    var steps: Array<Step> = [];
    for (r in reasons) {
      switch (Step.add(steps, r, book)) {
        case Error(e):
          steps = [];
          throw(new haxe.Exception(e));
        case Ok(ss): steps = ss;
      }
    }

    final div = Q("div");
    new DemoWg(div, book, steps);
    demoDiv
      .removeAll()
      .add(Q("hr"))
      .add(div)
    ;
  }

  function saveBook (): Void {
    Cts.client.send([
      "source" => Js.ws("Theorems"),
      "rq" => Js.ws("save"),
      "book" => book.toJs()
    ], rp -> {
      Cts.wait.show(false);
      mk(wg);
    });
  }

  function interchange (id1: BookId, id2: BookId): Void {
    Cts.wait.show(true);
    book.interchange(id1, id2);
    saveBook();
  }

  function remove (id: String): Void {
    if (!Ui.confirm(_args(_("Remove theorem '%0'?"), [id]))) return;
    Cts.wait.show(true);
    final used = book.used(id);
    if (used.length > 0) {
      Cts.wait.show(false);
      Ui.alert(_args(_("Theorem '%0' is used by:\n%1"), [
          id,
          used.length > 5
            ? It.join(It.from(used).take(5), ", ") + " ..."
            : used.join(", ")
        ]
      ));
      return;
    }
    book.remove(id);
    saveBook();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("Theorems"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final book = Book.fromJs(rp["book"]);
      final name = rp["name"].rs();
      new TheoremsPg(wg, book, name).show();
    });
  }
}
