// Copyright 30-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget.fixProblem;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import data.Plan;
import data.Diary; // Diary.Entry Diary.Annotation
import data.Cash; // CashEntry
import data.Problem;
import I18n._;
import I18n._args;

class New implements ProblemEditor {
  final tr: Domo;
  final plan: Plan;
  final hcEntry: CashEntry;
  final cEntry: DiaryEntry;
  final ix: Int;
  final deactivateAllFn: Void -> Void;
  final updateFn: (Int, DiaryEntry) -> Void;

  final imgDiv = Q("div");
  final annsDiv = Q("div");
  final annsEd: AnnsEditor;

  var activated(default, null): Bool;

  public function new (
    tr: Domo, plan: Plan, hcEntry: CashEntry, cEntry: DiaryEntry, ix:Int,
    activated: Bool,
    deactivateAllFn: Void -> Void,
    updateFn: (Int, DiaryEntry) -> Void
  ) {
    this.tr = tr;
    this.plan = plan;
    this.hcEntry = hcEntry;
    this.cEntry = cEntry;
    this.ix = ix;
    this.activated = activated;
    this.deactivateAllFn = deactivateAllFn;
    this.updateFn = updateFn;

    annsEd = new AnnsEditor(annsDiv, cEntry, update);

    view();
  }
  // View ----------------------------------------------------------------------

  function view (): Void {
    if (activated) {
      imgDiv
        .removeAll()
        .klass("frame")
        .add(Ui.lightImg("insert"))
      ;
      annsEd.active(true);
    } else {
      imgDiv
        .removeAll()
        .add(Ui.link(() -> activate())
          .klass("link")
          .add(Ui.img("insert")))
      ;
      annsEd.active(false);
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
        .klass("frameTx")
        .text(hcEntry.month))
      .add(Q("td")
        .klass("frameTx")
        .text(hcEntry.desc))
      .add(Q("td")
        .klass("frameNm")
        .text(Dec.toIso(hcEntry.am, 2)))
    ;
  }

  // Control -------------------------------------------------------------------

  public function deactivate (): Void {
    activated = false;
    view();
  }

  public function updateAccount (acc: String): Void {
    if (activated) {
      annsEd.updateAccount(acc);
    }
  }

  function activate (): Void {
    deactivateAllFn();
    activated = true;
    view();
  }

  function update (anns: Array<DiaryAnnotation>): Void {
    cEntry.setAnns(anns);
    updateFn(ix, cEntry);
  }

}
