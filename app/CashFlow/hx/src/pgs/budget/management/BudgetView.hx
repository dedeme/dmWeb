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
import dm.ModalBox;
import data.Plan;
import data.Diary; // DiaryEntry
import data.Budget;
import data.Month;
import I18n._;
import I18n._args;

class BudgetView {
  final wg: Domo;
  final selectedYear: String;
  final plan: Plan;
  final diary: Diary;
  final budget: Budget;
  final budgetModel: Array<BudgetEntry> = [];
  final diaryEntries: Array<Tp<Int, DiaryEntry>>;

  final accBox: ModalBox;
  final accWg = Q("div");

  public function new (
    wg: Domo, selectedYear: String, isUntil: Bool, selectedMonth: Int,
    plan: Plan, diary: Diary,
    budget: Budget
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.plan = plan;
    this.diary = diary;
    this.budget = budget;

    final start = isUntil ? 0 : selectedMonth -1;

    this.diaryEntries = diary.filterReverse(start, selectedMonth);

    for (pe in plan.entries) {
      budgetModel.push(new BudgetEntry (
        pe.isIncome, pe.id,
        budget.accAmount(pe.id, start, selectedMonth),
        diary.accAmount(pe.id, start, selectedMonth)
      ));
    }

    accBox = new ModalBox(accWg);

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
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
              .att("colspan", "4")
              .style("text-align:center")
              .text(_("Incomes")))
            .add(Q("td")
              .text(""))
            .add(Q("td")
              .att("colspan", "4")
              .style("text-align:center")
              .text(_("Expenses"))))
          .add(Q("tr")
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Account")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Budget (A)")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Real (B)")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Dif. (B - A))")))
            .add(Q("td")
              .klass("frameTx")
              .text(""))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Account")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Budget (A)")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Real (B)")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Dif. (B - A))"))))
          .adds(It.range(len).map(i ->
              Q("tr")
                .adds(i >= incomes.length
                  ? It.range(4).map(j ->
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9")
                    ).to()
                  : [
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#d9d9d9")
                        .add(Ui.link(() -> account(incomes[i].accId))
                          .klass("link")
                          .att("title", plan.desc(incomes[i].accId))
                          .text(incomes[i].accId)),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(incomes[i].budget, 2)),
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
                  ? It.range(4).map(j ->
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#f9f9f9")
                    ).to()
                  : [
                      Q("td")
                        .klass("frameTx")
                        .style("background-color:#d9d9d9")
                        .add(Ui.link(() -> account(expenses[i].accId))
                          .klass("link")
                          .att("title", plan.desc(expenses[i].accId))
                          .text(expenses[i].accId)),
                      Q("td")
                        .klass("frameNm")
                        .style("background-color:#f9f9f9")
                        .text(Dec.toIso(expenses[i].budget, 2)),
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
              .text(Dec.toIso(BudgetEntry.sumBudget(expenses), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumReal(expenses), 2)))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#f9f9f9")
              .text(Dec.toIso(BudgetEntry.sumDif(expenses), 2)))))

      // DIARY

      .add(Q("div")
        .style("padding:5px;text-align:center")
        .html("<hr><br>" + _("Diary")))
        .add(Q("table")
          .att("align", "center")
          .klass("summary")
          .add(Q("tr")
            .add(Q("td"))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Month")))
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(_("Description")))
            .add(Q("td")
              .klass("frameNm")
              .style("background-color:#d9d9d9")
              .text(_("Amount"))))
          .adds(diaryEntries.map(tp ->
              Q("tr")
                .add(Q("td")
                  .add(Ui.link(() -> delete(tp))
                    .add(Ui.img("delete"))))
                .add(Q("td")
                  .klass("frameTx")
                  .style("background-color:#f9f9f9")
                  .text(tp.e2.month))
                .add(Q("td")
                  .klass("frameTx")
                  .style("background-color:#f9f9f9")
                  .add(Q("table")
                    .add(Q("tr")
                      .add(Q("td")
                        .att("colspan", "2")
                        .text(tp.e2.desc)))
                    .adds(tp.e2.anns.map(a ->
                        Q("tr")
                          .add(Q("td")
                            .klass("frameTx")
                            .att("title", plan.desc(a.accId))
                            .text(a.accId))
                          .add(Q("td")
                            .klass("frameNm")
                            .text(Dec.toIso(a.am, 2)))
                      ))
                    ))
                .add(Q("td")
                  .klass("frameNm")
                  .style("background-color:#f9f9f9")
                  .text(Dec.toIso(tp.e2.am, 2)))
            )))

      // ACCOUNT

      .add(accBox.wg)
    ;
  }

// Control ---------------------------------------------------------------------

  function updateDiary (): Void {
    Cts.client.ssend([
      "source" => Js.ws("BudgetView"),
      "rq" => Js.ws("updateDiary"),
      "year" => Js.ws(selectedYear),
      "diary" => diary.toJs()
    ], rp -> {
      js.Browser.location.reload();
    });
  }

  function account (id: String): Void {
    final budgetMs: Array<Float> = [];
    final budgetSs: Array<Float> = [];
    var budgetSum = 0.0;
    final realMs: Array<Float> = [];
    final realSs: Array<Float> = [];
    var realSum = 0.0;
    for (i in 0...12) {
      final b = budget.accAmount(id, i, i + 1);
      budgetSum += b;
      budgetMs.push(b);
      budgetSs.push(budgetSum);
      final r = diary.accAmount(id, i, i + 1);
      realSum += r;
      realMs.push(r);
      realSs.push(realSum);
    }

    accWg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(id))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(Q("td")
            .att("colspan", "2")
            .style("text-align:center")
            .text(_("Budget (A)")))
          .add(Q("td"))
          .add(Q("td")
            .att("colspan", "2")
            .style("text-align:center")
            .text(_("Real (B)")))
          .add(Q("td"))
          .add(Q("td")
            .att("colspan", "2")
            .style("text-align:center")
            .text(_("Dif. (B - A))"))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Month")))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Sum")))
          .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Month")))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Sum")))
          .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Month")))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#d9d9d9")
            .text(_("Sum"))))
        .adds(It.range(12).map(i ->
          Q("tr")
            .add(Q("td")
              .klass("frameTx")
              .style("background-color:#d9d9d9")
              .text(Month.name(i + 1)))
            .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(budgetMs[i], 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(budgetSs[i], 2)))
          .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(realMs[i], 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(realSs[i], 2)))
          .add(Q("td"))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(budgetMs[i] - realMs[i], 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(budgetSs[i] - realSs[i], 2))))))
      .add(Q("div")
        .klass("head")
        .add(Q("button")
          .text(_("Accept"))
          .on(CLICK, () -> accBox.show(false))))
    ;

    accBox.show(true);
  }

  function delete (ann: Tp<Int, DiaryEntry>) {
    if (!Ui.confirm(_args(_("Delete '%0'?"), [ann.e2.desc]))) {
      return;
    }

    diary.delete(ann.e1);
    updateDiary();
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

