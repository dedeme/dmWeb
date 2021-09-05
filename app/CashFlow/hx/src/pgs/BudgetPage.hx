// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import data.Plan;
import data.Diary;
import data.Cash;
import data.Problem;
import data.Budget;
import pgs.budget.FixProblem;
import pgs.budget.Management;
import I18n._;
import I18n._args;

class BudgetPage {
  final wg: Domo;
  final selectedYear: String;
  final isUntil: Bool;
  final selectedMonth: Int;
  final plan: Plan;
  final hcBalance: Float;
  var cBalance: Float;
  final hcDiary: Cash;
  final cDiary: Diary;
  final budget: Budget;
  final previousBudget: Budget;

  function new (
    wg: Domo, selectedYear: String, isUntil: Bool, selectedMonth: Int,
    plan: Plan,
    hcBalance: Float, cBalance: Float, hcDiary: Cash, cDiary: Diary,
    budget: Budget, previousBudget: Budget
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.isUntil = isUntil;
    this.selectedMonth = selectedMonth;
    this.plan = plan;
    this.hcBalance = hcBalance;
    this.cBalance = cBalance;
    this.hcDiary = hcDiary;
    this.cDiary = cDiary;
    this.budget = budget;
    this.previousBudget = previousBudget;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final problem = Problem.firstProblem(hcDiary, cDiary);
    if (!Dec.eq(hcBalance, cBalance, 0.001)) {
      fixBalance();
    } else if (problem.ix != -1) {
      new FixProblem(wg, selectedYear, plan, cDiary, problem, () -> view());
    } else {
      new Management(
        wg, selectedYear, isUntil, selectedMonth,
        plan, cBalance, cDiary, budget, previousBudget
      );
    }
  }

  // Control -------------------------------------------------------------------

  function fixBalance (): Void {
    final hc = Dec.toIso(hcBalance, 2);
    final c = Dec.toIso(cBalance, 2);
    Ui.alert(_args(_("fixBalance hc(%0) c(%1)"), [hc, c]));

    cBalance = hcBalance;
    Cts.client.ssend([
      "source" => Js.ws("Budget"),
      "rq" => Js.ws("updateBalance"),
      "year" => Js.ws(selectedYear),
      "value" => Js.wf(cBalance)
    ], rp -> {
      view();
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg          : Widget.
  ///   selectedYear: Selected year.
  ///   isUntil: If the budget includes previous months.
  ///   selectedMonth: In range 1-12.
  public static function mk (
    wg: Domo, selectedYear: String, isUntil: Bool, selectedMonth: Int
  ): Void {
    Cts.client.send([
      "source" => Js.ws("Budget"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(selectedYear)
    ], rp -> {
      final plan = Plan.fromJs(rp["plan"]);
      final hcBalance = rp["hcBalance"].rf();
      final cBalance = rp["cBalance"].rf();
      final hcDiary = Cash.fromJs(rp["hcDiary"]);
      final cDiary = Diary.fromJs(rp["cDiary"]);
      final budget = Budget.fromJs(rp["budget"]);
      final previousBudget = Budget.fromJs(rp["previousBudget"]);
      budget.cleanAndComplete(plan);
      new BudgetPage(
        wg, selectedYear, isUntil, selectedMonth, plan,
        hcBalance, cBalance, hcDiary, cDiary,
        budget, previousBudget
      );
    });
  }

}
