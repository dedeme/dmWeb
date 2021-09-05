// Copyright 01-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget.fixProblem;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.It;
import data.Diary; // Diary.Entry Diary.Annotation
import data.Cash; // CashEntry
import data.Problem;
import I18n._;
import I18n._args;

class Delete implements ProblemEditor {
  final tr: Domo;
  final cEntry: DiaryEntry;
  final ix: Int;
  final deactivateAllFn: Void -> Void;
  final updateFn: Int -> Void;

  final imgDiv = Q("div");
  final annsDiv = Q("div");
  final delEd: DeleteEditor;

  var activated(default, null): Bool;

  public function new (
    tr: Domo, cEntry: DiaryEntry, ix: Int,
    activated: Bool,
    deactivateAllFn: Void -> Void,
    updateFn: Int -> Void
  ) {
    this.tr = tr;
    this.cEntry = cEntry;
    this.ix = ix;
    this.activated = activated;
    this.deactivateAllFn = deactivateAllFn;
    this.updateFn = updateFn;

    delEd = new DeleteEditor(annsDiv, cEntry, update);

    view();
  }
  // View ----------------------------------------------------------------------

  function view (): Void {
    if (activated) {
      imgDiv
        .removeAll()
        .klass("frame")
        .add(Ui.lightImg("delete"))
      ;
      delEd.active(true);
    } else {
      imgDiv
        .removeAll()
        .add(Ui.link(() -> activate())
          .klass("link")
          .add(Ui.img("delete")))
      ;
      delEd.active(false);
    }

    tr
      .removeAll()
      .add(Q("td")
        .add(imgDiv))
      .add(Q("td")
        .klass("frameTx")
        .text(cEntry.month))
      .add(Q("td")
        .klass("frameTx")
        .add(Q("div")
          .text(cEntry.desc))
        .add(annsDiv))
      .add(Q("td")
        .klass("frameNm")
        .text(Dec.toIso(cEntry.am, 2)))

      .add(Q("td"))

      .add(Q("td")
        .text(""))
      .add(Q("td")
        .text(""))
      .add(Q("td")
        .text(""))
    ;
  }

  // Control -------------------------------------------------------------------

  public function deactivate (): Void {
    activated = false;
    view();
  }

  public function updateAccount (acc: String): Void {
    // Do nothing
  }

  function activate (): Void {
    deactivateAllFn();
    activated = true;
    view();
  }

  function update (): Void {
    updateFn(ix);
  }

}

class DeleteEditor {
  final wg: Domo;
  final fnOk: Void -> Void;

  final am: Float;
  final anns: Array<DiaryAnnotation>;

  var activated(default, null): Bool;



  public function new (
    wg: Domo, entry: DiaryEntry, fnOk: Void -> Void
  ) {
    this.wg = wg;
    this.fnOk = fnOk;
    am = entry.am;
    anns = entry.anns;

    activated = false;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    wg.removeAll();
    if (!activated) return;

    wg
      .add(Q("table")
      .klass("main")
      .adds(It.range(anns.length).map(i ->
          Q("tr")
            .add(Q("td")
              .klass("frameTx")
              .text(anns[i].accId == "" ? "---" : anns[i].accId))
            .add(Q("td")
              .klass("frameNm")
              .text(Dec.toIso(anns[i].am, 2)))
        ))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td"))
        .add(Q("td")
          .style("text-align: right")
          .add(Ui.link(ok).add(Ui.img("ok"))))))
    ;
  }

  // Control -------------------------------------------------------------------

  public function active (value: Bool): Void {
    activated = value;
    view();
  }

  public function ok (): Void {
    var sum = 0.0;
    var ix = 1;
    for (a in anns) {
      if (a.accId == "") {
        Ui.alert(_args(
          _("Account of annotation '%0' is missing"), [Std.string(ix)]
        ));
        return;
      }
      sum += a.am;
      ++ix;
    }

    if (!Dec.eq(sum, am, 0.001)) {
      Ui.alert(_args(
        _("Sum of annotations (%0) does not match the cash value (%1)"),
        [Dec.toIso(sum, 2), Dec.toIso(am, 2)]
      ));
      return;
    }

    fnOk();
  }

}

