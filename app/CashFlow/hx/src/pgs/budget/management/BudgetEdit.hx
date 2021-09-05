// Copyright 04-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget.management;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Dec;
import dm.Opt;
import dm.Tp;
import dm.Js;
import data.Plan;
import data.Diary; // DiaryEntry
import data.Budget;
import wgs.NumberField;
import I18n._;

class BudgetEdit {
  final wg: Domo;
  final selectedYear: String;
  final selectedMonth: Int;
  final plan: Plan;
  final diary: Diary;
  final budget: Budget;
  final previousBudget: Budget;
  final updateFinalBalanceFn: Void -> Void;

  public function new (
    wg: Domo, selectedYear: String, selectedMonth: Int,
    plan: Plan, diary: Diary,
    budget: Budget, previousBudget: Budget,
    updateFinalBalanceFn: Void -> Void
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.selectedMonth = selectedMonth;
    this.plan = plan;
    this.diary = diary;
    this.budget = budget;
    this.previousBudget = previousBudget;
    this.updateFinalBalanceFn = updateFinalBalanceFn;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final diaryEntries = diary.filterReverse(selectedMonth -1, selectedMonth);

    final budgetModel: Array<BudgetEntry> = [];
    for (pe in plan.entries) {
      budgetModel.push(new BudgetEntry (
        pe.isIncome, pe.id,
        budget.accAmount(pe.id, selectedMonth -1, selectedMonth),
        diary.accAmount(pe.id, selectedMonth -1, selectedMonth)
      ));
    }

    final incomes = budgetModel.filter(e -> e.isIncome);
    incomes.sort((e1, e2) -> dm.Str.compare(e1.accId, e2.accId));
    final expenses = budgetModel.filter(e -> !e.isIncome);
    expenses.sort((e1, e2) -> dm.Str.compare(e1.accId, e2.accId));
    final len =
      incomes.length > expenses.length ? incomes.length : expenses.length;

    wg
      .removeAll()
      .add(Q("div")
        .style("padding:5px;text-align:center")
        .html("<hr><br>" + _("Budget")))
        .add(Q("table")
          .att("align", "center")
          .klass("summary")
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", "5")
              .style("text-align:center")
              .text(_("Incomes")))
            .add(Q("td")
              .text(""))
            .add(Q("td")
              .att("colspan", "5")
              .style("text-align:center")
              .text(_("Expenses"))))
          .add(Q("tr")
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Account")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(""))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Budget (A)")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Real (B)")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Dif. (B - A))")))
            .add(Q("td")
              .klass("frameTx")
              .text(""))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Account")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(""))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Budget (A)")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Real (B)")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9;white-space: nowrap;")
              .text(_("Dif. (B - A))"))))
          .adds(It.range(len).map(i ->
              Q("tr")
                .adds(i >= incomes.length
                  ? It.range(5).map(j ->
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9")
                    ).to()
                  : [
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#d9d9d9")
                        .att("title", plan.desc(incomes[i].accId))
                        .text(incomes[i].accId),
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9;white-space: nowrap;")
                        .add(Ui.link(() -> getReal(incomes[i].accId))
                          .klass("link")
                          .text(_("real")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(Ui.link(() -> getMonth(incomes[i].accId))
                          .klass("link")
                          .text(_("month")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(Ui.link(() -> getYear(incomes[i].accId))
                          .klass("link")
                          .text(_("year")))
                          ,
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .add(new NumberField(
                            incomes[i].accId, expenses[0].accId,
                            incomes[i].budget, changeAmount
                          ).wg),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(incomes[i].real, 2)),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(incomes[i].dif, 2)),
                    ])
                .add(Q("td")
                  .klass("frameTx"))
                .adds(i >= expenses.length
                  ? It.range(5).map(j ->
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9")
                    ).to()
                  : [
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#d9d9d9")
                        .att("title", plan.desc(expenses[i].accId))
                        .text(expenses[i].accId),
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9;white-space: nowrap;")
                        .add(Ui.link(() -> getReal(expenses[i].accId))
                          .klass("link")
                          .text(_("real")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(Ui.link(() -> getMonth(expenses[i].accId))
                          .klass("link")
                          .text(_("month")))
                        .add(Q("span").html("&nbsp;&nbsp;"))
                        .add(Ui.link(() -> getYear(expenses[i].accId))
                          .klass("link")
                          .text(_("year")))
                          ,
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .add(new NumberField(
                            expenses[i].accId, incomes[0].accId,
                            expenses[i].budget, changeAmount
                          ).wg),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(expenses[i].real, 2)),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(expenses[i].dif, 2)),
                    ])
            ).to())
          .add(Q("tr")
            .add(Q("td")
              .att("colsapan", "9")))
          .add(Q("tr")
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Total")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(""))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumBudget(incomes), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumReal(incomes), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumDif(incomes), 2)))
            .add(Q("td")
              .klass("frameTx"))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Total")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(""))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumBudget(expenses), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumReal(expenses), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumDif(expenses), 2)))))
   ;
  }

// Control ---------------------------------------------------------------------

  function updateBudget (): Void {
    Cts.client.ssend([
      "source" => Js.ws("BudgetEdit"),
      "rq" => Js.ws("updateBudget"),
      "year" => Js.ws(selectedYear),
      "budget" => budget.toJs(),
    ], rp -> {
      updateFinalBalanceFn();
      view();
    });
  }

  function updateDiary (): Void {
    Cts.client.ssend([
      "source" => Js.ws("BudgetEdit"),
      "rq" => Js.ws("updateDiary"),
      "year" => Js.ws(selectedYear),
      "diary" => diary.toJs()
    ], rp -> {
      updateFinalBalanceFn();
      view();
    });
  }

  function getReal (id: String): Void {
    budget.setAmount(
      selectedMonth - 1,
      id,
      diary.accAmount(id, selectedMonth - 1, selectedMonth)
    );
    updateBudget();
  }

  function getMonth (id: String): Void {
    var b = budget;
    var m = selectedMonth - 1;
    if (m < 1) {
      b = previousBudget;
      m = 12;
    }
    final am = b.accAmount(id, m - 1, m);
    if (am == 0) {
      Ui.alert(_("There is not value for previous month"));
      view();
      Q("#" + id).e.focus();
      return;
    }
    budget.setAmount(selectedMonth - 1, id, am);
    updateBudget();
  }

  function getYear (id: String): Void {
    final am = previousBudget.accAmount(id, selectedMonth - 1, selectedMonth);
    if (am == 0) {
      Ui.alert(_("There is not value for previous year"));
      view();
      Q("#" + id).e.focus();
      return;
    }
    budget.setAmount(selectedMonth - 1, id, am);
    updateBudget();
  }

  function changeAmount (id: String, am: Float) {
    if (am < 0) {
      Ui.alert(_("Values less than 0 are not allowed"));
      view();
      Q("#" + id).e.focus();
      return;
    }
    budget.setAmount(selectedMonth - 1, id, am);
    updateBudget();
  }
}

private class BudgetEntry {
  public final isIncome: Bool;
  public final accId: String;
  public final budget: Float;
  public final real: Float;
  public var dif(default, null): Float;

  public function new (
    isIncome: Bool, accId: String, budget: Float, real: Float
  ) {
    this.isIncome = isIncome;
    this.accId = accId;
    this.budget = budget;
    this.real = real;
    dif = real - budget;
  }

  public static function sumBudget (entries: Array<BudgetEntry>): Float {
    var sum = 0.0;
    for (e in entries) sum += e.budget;
    return sum;
  }

  public static function sumReal (entries: Array<BudgetEntry>): Float {
    var sum = 0.0;
    for (e in entries) sum += e.real;
    return sum;
  }

  public static function sumDif (entries: Array<BudgetEntry>): Float {
    var sum = 0.0;
    for (e in entries) sum += e.dif;
    return sum;
  }

}
