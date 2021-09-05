// Copyright 02-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.budget;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Dec;
import dm.Menu;
import data.Plan;
import data.Diary;
import data.Budget;
import data.Month;
import pgs.budget.management.BudgetView;
import pgs.budget.management.BudgetEdit;
import I18n._;

/// Budget management.
class Management {
  final wg: Domo;
  final selectedYear: String;
  final isUntil: Bool;
  final selectedMonth: Int; // 1-12
  final plan: Plan;
  final balance: Float;
  final diary: Diary;
  final budget: Budget;
  final previousBudget: Budget;

  final monthName: String;
  var isEdition = false;

  final finalBalanceWg = Q("div");
  final budgetWg = Q("div");

  public function new (
    wg: Domo, selectedYear: String, isUntil: Bool, selectedMonth: Int,
    plan: Plan,
    balance: Float, diary: Diary,
    budget: Budget, previousBudget: Budget
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.isUntil = isUntil;
    this.selectedMonth = selectedMonth;
    this.plan = plan;
    this.balance = balance;
    this.diary = diary;
    this.budget = budget;
    this.previousBudget = previousBudget;

    monthName = Month.name(selectedMonth);

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final lopts = [
      isUntil
        ? Menu.toption("", _("Monthly"), changeAccumulation)
        : Menu.toption("", _("Accumulated"), changeAccumulation),
      Menu.separator2()
    ];
    It.from(_("months").split(","))
      .eachIx((m, i) -> {
        if (i > 0) lopts.push(Menu.separator());
        lopts.push(Menu.toption(
          Month.name(i + 1), Month.name(i + 1), () -> changeMonth(i + 1)
        ));
      });
    final ropts = [
      isEdition
        ? Menu.toption("", _("View"), changeEdition)
        : Menu.toption("", _("Edit"), changeEdition)
    ];
    final menu = new Menu(lopts, ropts, monthName);

    finalBalance();

    if (isEdition) {
      new BudgetEdit(
        budgetWg, selectedYear, selectedMonth, plan, diary,
        budget, previousBudget,
        finalBalance
      );
    } else {
      new BudgetView(
        budgetWg, selectedYear, isUntil, selectedMonth, plan, diary, budget
      );
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .html(
            (isUntil
              ? _("Accumulated Budget")
              : _("Monthly Budget")) +
            "<br> [ " +
            (isEdition
              ? _("Edit")
              : _("View")) +
            " ]")
          )
      .add(Q("div")
        .style("padding:5px;text-align:center")
        .text(_("Initial Balance")))
      .add(initialBalance())
      .add(Q("div")
        .style("padding:5px;text-align:center")
        .text(_("Final Balance")))
      .add(finalBalanceWg)
      .add(budgetWg)
    ;
  }

  function initialBalance (): Domo {
    return Q("table")
      .klass("summary")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("frameNm")
          .style("background-color:#f9f9f9")
          .text(Dec.toIso(isUntil ? balance : 0, 2))))
    ;
  }

  function finalBalance () {
    final month = Month.format(selectedMonth);
    final real = diary.totalAmount(
      plan, isUntil ? 0 : selectedMonth - 1, selectedMonth
    );
    final budgeted = budget.totalAmount(
      plan, isUntil ? 0 : selectedMonth - 1, selectedMonth
    );

    finalBalanceWg
      .removeAll()
      .add(Q("table")
        .klass("summary")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td"))
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
        .add(Q("tr")
          .add(Q("td")
            .klass("frameTx")
            .style("background-color:#d9d9d9")
            .text(_("Operations")))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(budgeted, 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(real, 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text("")))
        .add(Q("tr")
          .add(Q("td")
            .klass("frameTx")
            .style("background-color:#d9d9d9")
            .text(_("Total (In - Ex)")))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso((isUntil ? balance : 0) + budgeted, 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso((isUntil ? balance : 0) + real, 2)))
          .add(Q("td")
            .klass("frameNm")
            .style("background-color:#f9f9f9")
            .text(Dec.toIso(real - budgeted, 2)))))
    ;

  }

  // Control -------------------------------------------------------------------

  function changeAccumulation () {
    js.Browser.location.assign(
      "?budget&" + selectedYear + "&" + !isUntil + "&" + selectedMonth
    );
  }

  function changeMonth (m: Int) {
    js.Browser.location.assign(
      "?budget&" + selectedYear + "&" + isUntil + "&" + m
    );
  }

  function changeEdition () {
    isEdition = !isEdition;
    view();
  }

}
