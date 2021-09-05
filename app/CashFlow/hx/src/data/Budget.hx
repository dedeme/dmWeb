// Copyright 02-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import data.Plan;

/// Annual budget.
class Budget {
  public final months: Array<BudgetMonth>;

  function new (months: Array<BudgetMonth>) {
    this.months = months;
  }

  public function setAmount (monthIx: Int, accId: String, am: Float): Void {
    final accs = months[monthIx].accs;
    for (i in 0...accs.length) {
      if (accs[i].accId == accId) {
        accs[i] = new BudgetAnnotation(accId, am);
        break;
      }
    }
  }

  public function cleanAndComplete (plan: Plan): Void {
    for (i in 0...12) {
      months[i].cleanAndComplete(plan);
    }
  }

  /// The return is ever positive.
  public function accAmount (
    accId: String, fromMonthIx: Int, toMonthIx: Int
  ): Float {
    var sum = 0.0;
    for (i in fromMonthIx...toMonthIx) {
      final month = months[i];
      for (a in month.accs) {
        if (a.accId == accId) sum += a.am;
      }
    }
    return sum;
  }

  public function totalAmount (
    plan: Plan, fromMonthIx: Int, toMonthIx: Int
  ) {
    var sum = 0.0;
    for (i in fromMonthIx...toMonthIx) {
      final month = months[i];
      for (a in month.accs) {
        if (plan.isIncome(a.accId)) sum += a.am;
        else sum -= a.am;
      }
    }
    return sum;
  }

  public function toJs (): Js {
    return Js.wa(months.map(bm -> bm.toJs()));
  }

  // static --------------------------------------------------------------------

  public static function mkEmpty (plan: Plan): Budget {
    return new Budget(It.range(12).map(i -> BudgetMonth.mkEmpty(plan)).to());
  }


  public static function fromJs (js: Js): Budget {
    return new Budget(js.ra().map(e -> BudgetMonth.fromJs(e)));
  }
}

/// Monthly budget.
class BudgetMonth {
  public final accs: Array<BudgetAnnotation>;

  function new (accs: Array<BudgetAnnotation>) {
    this.accs = accs;
  }

  public function cleanAndComplete (plan: Plan): Void {
    final newAccs = plan.entries
      .map(e -> {
          for (acc in accs) {
            if (acc.accId == e.id) {
              return new BudgetAnnotation(e.id, acc.am);
            }
          }
          return new BudgetAnnotation(e.id, 0);
        });
    accs.splice(0, accs.length);
    for (acc in newAccs) accs.push(acc);
  }

  public function toJs (): Js {
    return Js.wa(accs.map(a -> a.toJs()));
  }

  // static --------------------------------------------------------------------
  public static function mkEmpty (plan: Plan) {
    return new BudgetMonth(plan.entries.map(e ->
        new BudgetAnnotation(e.id, 0.0)
      ));
  }

  public static function fromJs (js: Js): BudgetMonth {
    return new BudgetMonth(js.ra().map(e -> BudgetAnnotation.fromJs(e)));
  }

}

/// Budget annotation
class BudgetAnnotation {
  public final accId: String;
  public final am: Float;

  public function new (accId: String, am: Float) {
    this.accId = accId;
    this.am = am;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(accId),
      Js.wf(am)
    ]);
  }

  public static function fromJs (js: Js): BudgetAnnotation {
    final a = js.ra();
    return new BudgetAnnotation(
      a[0].rs(),
      a[1].rf()
    );
  }
}

