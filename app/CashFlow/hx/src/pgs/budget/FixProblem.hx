// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Opt;
import data.Plan;
import data.Diary;
import data.Cash;
import data.Problem;
import I18n._;
import I18n._args;
import pgs.budget.fixProblem.ProblemEditor;
import pgs.budget.fixProblem.New;
import pgs.budget.fixProblem.Delete;
import pgs.budget.fixProblem.Modify;

/// Fix problem page.
class FixProblem {
  final DEL_TR = 0;
  final NEW_TR = 1;
  final MODIFY_TR = 2;

  final wg: Domo;
  final selectedYear: String;
  final plan: Plan;
  final diary: Diary;
  final problem: Problem;
  final fnReload: () -> Void;

  final trs = [Q("tr"), Q("tr"), Q("tr")];
  final trWgs: Array<Option<ProblemEditor>> =
    [None, None, None]; // DEL, MODIFY, NEW

  var updateLocked = false;

  public function new (
    wg: Domo, selectedYear: String,
    plan: Plan, diary: Diary, problem: Problem,
    fnReload: () -> Void
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.plan = plan;
    this.diary = diary;
    this.problem = problem;
    this.fnReload = fnReload;

    switch (problem.hcErr) {
    case None:
      switch (problem.cErr) {
      case None:
      case Some(e):
        trWgs[DEL_TR] = Some(new Delete(
          trs[DEL_TR],
          diary.entries[problem.ix],
          problem.ix,
          true,
          deactivateAll, deleteDiaryEntry
        ));
      }
    case Some(hce):
      switch (problem.cErr) {
      case None:
        trWgs[NEW_TR] = Some(new New(
          trs[NEW_TR],
          plan,
          hce,
          new DiaryEntry(hce.month, hce.desc, hce.isIncome, [
              new DiaryAnnotation("", hce.am)
            ]),
          problem.ix,
          true,
          deactivateAll, addDiaryEntry
        ));
      case Some(ce):
        trWgs[DEL_TR] = Some(new Delete(
          trs[DEL_TR],
          diary.entries[problem.ix],
          problem.ix,
          false,
          deactivateAll, deleteDiaryEntry
        ));
        trWgs[NEW_TR] = Some(new New(
          trs[NEW_TR],
          plan,
          hce,
          new DiaryEntry(hce.month, hce.desc, hce.isIncome, [
              new DiaryAnnotation("", hce.am)
            ]),
          problem.ix,
          false,
          deactivateAll, addDiaryEntry
        ));
        trWgs[MODIFY_TR] = Some(new Modify(
          trs[MODIFY_TR],
          plan,
          hce,
          new DiaryEntry(hce.month, hce.desc, hce.isIncome,
            diary.entries[problem.ix].anns
          ),
          problem.ix,
          false,
          deactivateAll, modifyDiaryEntry
        ));
      }
    }


    view();
  }

  // View ----------------------------------------------------------------------

  function mkCtas (ctasDiv): Void {
    final es = switch(problem.hcErr) {
      case None: [];
      case Some(e): e.isIncome
        ? plan.entries.filter(e -> e.isIncome)
        : plan.entries.filter(e -> !e.isIncome)
      ;
    }
    return ctasDiv
      .add(Q("div")
        .style("padding-top:40px"))
      .add(Q("div")
        .klass("frame")
        .add(Q("table")
          .add(Q("tr")
            .add(Q("td")
              .html("<b>" + _("Accounts") + "</b>")))
          .add(Q("tr")
            .add(Q("td")
              .add(Q("hr"))))
          .adds(es.map(e ->
              Q("tr")
                .add(Q("td")
                  .add(Ui.link(() -> updateAccount(e.id))
                    .klass("link")
                    .att("title", e.desc)
                    .text(e.id)))
            ))))
    ;
  }

  function mkBody (bodyDiv): Void {
    function tdTx (tx: String): Domo {
      return Q("td")
        .klass("frameTx")
        .text(tx)
      ;
    }
    function tdNm (n: Float): Domo {
      return Q("td")
        .klass("frameNm")
        .text(Dec.toIso(n, 2))
      ;
    }

    final hcps = problem.hcPrevs;
    final cps = problem.cPrevs;
    final hcns = problem.hcNexts;
    final cns = problem.cNexts;

    return bodyDiv
      .add(Q("div")
        .klass("head")
        .text(_("Fix Problem")))
      .add(Q("table")
        .att("align", "center")
        .klass("summary")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "4")
            .style("text-align:center")
            .html("<b>CashFlow</b>"))
          .add(Q("td")
            .style("padding-left:5px"))
          .add(Q("td")
            .att("colspan", "3")
            .style("text-align:center")
            .html("<b>Hconta</b>")))
        .adds(It.range(5).map(i -> {
            final tr = Q("tr");
            tr.add(Q("td"));
            if (i < cps.length) {
              tr.add(tdTx(cps[i].month));
              tr.add(tdTx(cps[i].desc));
              tr.add(tdNm(cps[i].am));
            } else {
              It.range(3).each(i -> tr.add(tdTx("")));
            }
            tr.add(Q("td"));
            if (i < hcps.length) {
              tr.add(tdTx(hcps[i].month));
              tr.add(tdTx(hcps[i].desc));
              tr.add(tdNm(hcps[i].am));
            } else {
              It.range(3).each(i -> tr.add(tdTx("")));
            }
            return tr;
          }))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "10"))
            .style("height:15px"))
        .adds(It.range(3)
            .filter(i -> trWgs[i] != None)
            .map(i -> trs[i]).to()
          )
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "10"))
            .style("height:15px"))
        .adds(It.range(5).map(i -> {
            final tr = Q("tr");
            tr.add(Q("td"));
            if (i < cns.length) {
              tr.add(tdTx(cns[i].month));
              tr.add(tdTx(cns[i].desc));
              tr.add(tdNm(cns[i].am));
            } else {
              It.range(3).each(i -> tr.add(tdTx("")));
            }
            tr.add(Q("td"));
            if (i < hcns.length) {
              tr.add(tdTx(hcns[i].month));
              tr.add(tdTx(hcns[i].desc));
              tr.add(tdNm(hcns[i].am));
            } else {
              It.range(3).each(i -> tr.add(tdTx("")));
            }
            return tr;
          }))
          )
    ;
  }

  function view (): Void {
    final ctasDiv = Q("div");
    mkCtas(ctasDiv);
    final bodyDiv = Q("div");
    mkBody(bodyDiv);
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top")
            .add(ctasDiv))
          .add(Q("td")
            .style("vertical-align:top")
            .add(bodyDiv))))
    ;
  }

  // Control -------------------------------------------------------------------

  function updateServer (): Void {
    if (updateLocked) return;
    updateLocked = true;

    Cts.client.ssend([
      "source" => Js.ws("FixProblem"),
      "rq" => Js.ws("updateDiary"),
      "year" => Js.ws(selectedYear),
      "diary" => diary.toJs()
    ], rp -> {
      fnReload();
    });
  }

  function updateAccount (acc: String): Void {
    for (ow in trWgs) {
      switch (ow) {
      case Some(w):
        w.updateAccount(acc);
      case None:
      }
    }
  }

  function deactivateAll (): Void {
    It.range(3)
    .each(i -> switch (trWgs[i]) {
        case None:
        case Some(w): w.deactivate();
      });
  }

  function deleteDiaryEntry (ix: Int): Void {
    diary.entries.splice(ix, 1);
    updateServer();
  }

  function addDiaryEntry (ix: Int, e: DiaryEntry): Void {
    diary.entries.insert(ix, e);
    updateServer();
  }

  function modifyDiaryEntry (ix: Int, e: DiaryEntry): Void {
    diary.entries[ix] = e;
    updateServer();
  }
}



