// Copyright 25-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.Dec;
import dm.Store;
import dm.Menu;
import data.Report;
import data.All;
import data.FormRow;
import I18n._;

/// Form report.
class Form {
  static final KEY = "Stocks_forms_key";
  static final bk0 = "#c0c0c0";
  static final bk1 = "#f9f9ff";

  /// Constructor.
  ///   wg: Container.
  ///   year: Year of report. If it is None, report is for all years.
  public static function mk (wg:Domo, year: Option<String>) {
    All.request(all -> {
      show(wg, Report.ALL, all, year);
    });
  }

  // If isel == Report.ALL, data is from 'all'.
  // If isel == Report.WITH_FEES, data is from 'with'.
  // Otherwise is "Inv-" + isel
  static function show (
    wg: Domo, isel: Int, all: All, year: Option<String>
  ): Void {
    final sel = isel == Report.ALL
      ? "all"
      : isel == Report.WITH_FEES
        ? "with"
        : "Inv-" + Std.string(isel)
    ;

    final ylopts = [];
    ylopts.push(Menu.toption(
        "all", _("All"), () -> show(wg, isel, all, None)
    ));
    for (myear in all.yearIds()) {
      ylopts.push(Menu.separator());
      ylopts.push(Menu.toption(
        myear, myear, () -> show(wg, isel, all, Some(myear))
      ));
    }
    final yopsel = switch (year) {
      case Some(y): y;
      case None: "all";
    }
    final ymenu = new Menu(ylopts, [], yopsel);

    var lopts = [Menu.toption("all", _("All"), () -> show(wg, -1, all, year))];
    for (i in 0...Cts.investors) {
      final ix = i;
      final name = "Inv-" + Std.string(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(name, name, () -> show(wg, ix, all, year)));
    }
    var ropts = [
      Menu.toption("with", _("With Fees"), () -> show(wg, -2, all, year))
    ];
    final menu = new Menu(lopts, ropts, sel);

    final body = Q("div");
    final nicks = all.nicks(isel, year);
    var nickSel = Opt.get(Store.get(KEY));
    if (nickSel == null || !nicks.contains(nickSel)) nickSel = "";
    showNicks(body, isel, nicks, nickSel, all, year);

    wg
      .removeAll()
      .add(ymenu.wg)
      .add(menu.wg)
      .add(body)
    ;
  }

  static function mkRow(e: FormRow): Domo {
    final bkb = "#fff0e0";
    final bks = "#e0f0ff";
    final bkt = "#f0f0f0";
    return Q("tr")
      .add(Q("td")
        .klass("number2")
        .text(e.date))
      .add(Q("td"))
      .add(Q("td")
        .klass("number2")
        .style("background:" + bkb)
        .text(e.bs == 0 ? "" : Dec.toIso(e.bs, 0)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bkb)
        .text(e.bs == 0 ? "" : Dec.toIso(e.bp, 4)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bkb)
        .text(e.bs == 0 ? "" : Dec.toIso(e.bt, 2)))
      .add(Q("td"))
      .add(Q("td")
        .klass("number2")
        .style("background:" + bks)
        .text(e.ss == 0 ? "" : Dec.toIso(e.ss, 0)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bks)
        .text(e.ss == 0 ? "" : Dec.toIso(e.sp, 4)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bks)
        .text(e.ss == 0 ? "" : Dec.toIso(e.st, 2)))
      .add(Q("td"))
      .add(Q("td")
        .klass("number2")
        .style("background:" + bkt)
        .text(Dec.toIso(e.ts, 0)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bkt)
        .text(e.ts == 0 ? "" : Dec.toIso(e.tp, 4)))
      .add(Q("td")
        .klass("number")
        .style("background:" + bkt)
        .text(Dec.toIso(e.tt, 2)))
      .add(Q("td"))
      .add(Q("td"))
      .add(Q("td")
        .klass("number")
        .style("background:" + (e.profits == None ? bk0 : bk1))
        .text(e.profits == None ? "" : Dec.toIso(Opt.get(e.profits), 2)))
      .add(Q("td")
        .klass("number")
        .style("background:" + (e.fees == None ? bk0 : bk1))
        .text(e.fees == None ? "" : Dec.toIso(Opt.get(e.fees), 2)))
    ;
  }

  // type can be: Report.ALL, Report.WITH_FEES or number of investor.
  static function showNicks(
    wg: Domo, type: Int, nicks: Array<String>, nickSel: String, all: All,
    year: Option<String>
  ): Void {
    if (nicks.length == 0) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .text(_("Without data")))))
      ;
      return;
    }

    if (nickSel == "") nickSel = nicks[0];
    Store.put(KEY, nickSel);
    var lopts = [];
    var first = true;
    for (e in nicks) {
      if (first) first = false;
      else lopts.push(Menu.separator());
      lopts.push(Menu.toption(
        e, e, () -> showNicks(wg, type, nicks, e, all, year)
      ));
    }
    final menu = new Menu2(lopts, [], nickSel);

    final entries = all.form(type, nickSel, year);
    final ole = entries.length > 0
      ? Some(entries[entries.length - 1])
      : None
    ;

    final table = Q("table")
      .klass("border")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("header")
          .text(_("Date")))
        .add(Q("td").text(" "))
        .add(Q("td")
          .klass("header")
          .text(_("Stocks")))
        .add(Q("td")
          .klass("header")
          .text(_("Price")))
        .add(Q("td")
          .klass("header")
          .text(_("Total")))
        .add(Q("td").text(" "))
        .add(Q("td")
          .klass("header")
          .text(_("Stocks")))
        .add(Q("td")
          .klass("header")
          .text(_("Price")))
        .add(Q("td")
          .klass("header")
          .text(_("Total")))
        .add(Q("td").text(" "))
        .add(Q("td")
          .klass("header")
          .text(_("Stocks")))
        .add(Q("td")
          .klass("header")
          .text(_("Price")))
        .add(Q("td")
          .klass("header")
          .text(_("Total")))
        .add(Q("td").text(" "))
        .add(Q("td").text(" "))
        .add(Q("td")
          .klass("header")
          .text(_("Profits")))
        .add(Q("td")
          .klass("header")
          .text(_("Fees"))))
      .adds(entries.map(e -> mkRow(e)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "15"))
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(switch (ole) {
        case Some(le):
          Q("tr")
            .add(Q("td")
              .att("colspan", "14")
              .style("text-align: right")
              .text(_("Sum") + ":"))
            .add(Q("td").text(" "))
            .add(Q("td")
              .klass("number")
              .style("background:" + bk1)
              .text(Dec.toIso(le.ttProfits, 2)))
            .add(Q("td")
              .klass("number")
              .style("background:" + (le.ttFees > 0 ? bk1 : bk0))
              .text(le.ttFees > 0 ? Dec.toIso(le.ttFees, 2) : ""))
          ;
        case None:
          Q("tr")
            .add(Q("td")
              .att("colspan", "17")
              .style("text-align: center")
              .text(_("Without Data")))
          ;
        })
    ;

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(_("Form")))
      .add(table)
    ;
  }

}
