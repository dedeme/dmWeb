// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import data.Plan;
import data.Diary;
import I18n._;
import I18n._args;

/// Plan page.
class PlanPage {
  final wg: Domo;
  final year: String;
  final plan: Plan;
  final diary: Diary;
  var oldEntry: Option<PlanEntry>;

  function new (
    wg: Domo, year: String, plan: Plan, diary: Diary
  ) {
    this.wg = wg;
    this.year = year;
    this.plan = plan;
    this.diary = diary;
    oldEntry = None;

    view();
  }

  // View ----------------------------------------------------------------------

  function dataEntry (): Domo {
    final entry = switch(oldEntry) {
      case None: new PlanEntry(false, "", "");
      case Some(e): e;
    }

    final opInc = Q("input")
      .att("type", "radio")
      .checked(entry.isIncome)
      .att("name", "inc")
    ;
    final opExp = Q("input")
      .att("type", "radio")
      .checked(!entry.isIncome)
      .att("name", "inc")
    ;
    final idWg = Ui.field("desc")
      .att("id", "autofocus")
      .style("width:80px")
      .value(entry.id)
    ;
    final descWg = Ui.field("accept")
      .att("id", "desc")
      .style("width:200px")
      .value(entry.desc)
    ;

    function mkEntry (): PlanEntry {
      return new PlanEntry(
        opInc.getChecked(),
        Std.string(idWg.getValue()).trim().substring(0, 6),
        Std.string(descWg.getValue()).trim()
      );
    }

    function th (tx: String): Domo {
      return Q("td")
        .style(
            "background-color: #c9c9c9;text-align: center;" +
            "border: 1px solid rgb(110,130,150)"
          )
        .html(tx)
      ;
    }

    function tdInfo (tx: String): Domo {
      return Q("td")
        .style("color:#C9C9C9;text-align:center")
        .text(tx)
      ;
    }

    final table = Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(th(_("Type")).att("colspan", "5"))
        .add(th(_("Id")))
        .add(th(_("Description"))))
      .add(Q("tr")
        .add(Q("td").add(opInc))
        .add(Q("td").text(_("Income")))
        .add(Q("td").style("width: 5px"))
        .add(Q("td").add(opExp))
        .add(Q("td").text(_("Expense")))

        .add(Q("td").add(idWg))
        .add(Q("td").add(descWg)))
    ;

    switch (oldEntry) {
      case None:
      case Some(e):

        table
          .add(Q("tr")
            .add(tdInfo(e.isIncome ? _("Income") : _("Expense"))
              .att("colspan", "5"))
            .add(tdInfo(e.id))
            .add(tdInfo(e.desc)))
        ;
    }

    table
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "7")
          .style("text-align: right")
          .add(Q("button")
            .text(_("Cancel"))
            .on(CLICK, reload))
          .add(Q("span").html("&nbsp;"))
          .add(Q("button")
            .att("id", "accept")
            .text(_("Accept"))
            .on(CLICK, () -> acceptEntry(mkEntry())))))
    ;

    return table;
  }

  function entryWg (): Domo {
    return Q("div")
      .add(Q("div")
        .klass("head")
        .html(oldEntry == None ? _("New Account") : _("Modification")))
      .add(dataEntry())
    ;
  }

  function withoutAccountsTable (): Domo {
    return Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("text-align: center")
          .text(_("Without Accounts"))))
    ;
  }

  function mkTr(entry: PlanEntry): Domo {
    return Q("tr")
      .add(Q("td")
        .add(Ui.link(() -> delete(entry.id))
          .add(Ui.img("delete"))))
      .add(Q("td")
        .add(Ui.link(() -> modify(entry))
          .add(Ui.img("edit"))))
      .add(Q("td")
        .style("border: 1px solid rgb(110,130,150)")
        .text(entry.id))
      .add(Q("td")
        .style("border: 1px solid rgb(110,130,150)")
        .text(entry.desc))
    ;
  }

  function entriesTable (isIncomes: Bool): Domo {
    final div = Q("div")
      .add(Q("div")
        .klass("head")
        .html(isIncomes ? _("Incomes") : _("Expenses")))
    ;

    final es = plan.entries.filter(e -> e.isIncome == isIncomes);

    if (es.length == 0) {
      div.add(withoutAccountsTable());
    } else {
      final trs = [];
      for (e in es) {
        trs.push(mkTr(e));
      }
      div.add(Q("table")
        .att("align", "center")
        .adds(trs));
    }

    return div;
  }

  function view (): Void {
    plan.entries.sort((e1, e2) ->
      dm.Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html(_("Accounting Plan")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "2")
            .klass("frame")
            .style("text-align: center")
            .add(entryWg())))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .style("vertical-align: top; width:50%")
            .add(entriesTable(true)))
          .add(Q("td")
            .klass("frame")
            .style("vertical-align: top; width:50%")
            .add(entriesTable(false)))))
    ;

    Cts.autofocus();
  }

  // Control -------------------------------------------------------------------

  function updateServerPlan (fn: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("PlanPage"),
      "rq" => Js.ws("updatePlan"),
      "year" => Js.ws(year),
      "plan" => plan.toJs()
    ], rp -> {
      fn();
    });
  }

  function updateServerPlanAndDiary (fn: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("PlanPage"),
      "rq" => Js.ws("updatePlanAndDiary"),
      "year" => Js.ws(year),
      "plan" => plan.toJs(),
      "diary" => diary.toJs(),
    ], rp -> {
      fn();
    });
  }

  function reload (): Void {
    oldEntry = None;
    view();
  }

  function acceptEntry (entry: PlanEntry): Void {
    switch (oldEntry) {
      case None:
        final r = plan.add(entry);
        if (r != "") {
          Ui.alert(r);
          return;
        }
        updateServerPlan(reload);
      case Some(e):
        final r = plan.modify(e.id, entry, diary);
        if (r != "") {
          Ui.alert(r);
          return;
        }
        diary.changeAcc(e.id, entry.id);
        updateServerPlanAndDiary(reload);
    }
  }

  function delete (id: String) {
    if (!Ui.confirm(_args(_("Delete %0?"), [id]))) {
      return;
    }
    final r = plan.del(id, diary.accs());
    if (r != "") {
      Ui.alert(r);
      return;
    }
    updateServerPlan(reload);
  }

  function modify (entry: PlanEntry): Void {
    oldEntry = Some(entry);
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg          : Widget.
  ///   selectedYear: Selected year.
  public static function mk (wg: Domo, selectedYear: String): Void {
    Cts.client.send([
      "source" => Js.ws("PlanPage"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(selectedYear)
    ], rp -> {
      final plan = Plan.fromJs(rp["plan"]);
      final diary = Diary.fromJs(rp["diary"]);
      new PlanPage(wg, selectedYear, plan, diary);
    });
  }

}
