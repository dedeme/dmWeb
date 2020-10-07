// Copyright 25-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.Opt;
import dm.Menu;
import data.All;
import I18n._;
import I18n._args;

/// Treasure report.
class Treasury {
  /// Constructor.
  ///   wg: Container.
  public static function mk (wg:Domo) {
    All.request(all -> {
      final lastYear = all.lastYearId();
      final body = Q("div");
      final lopts = [];
      var first = true;
      for (year in all.yearIds()) {
        if (first) {
          first = false;
        } else {
          lopts.push(Menu.separator());
        }
        lopts.push(Menu.toption(
          year, year, () -> show(body, lastYear, all)
        ));
      }
      final menu = new Menu(lopts, [], lastYear);

      show(body, lastYear, all);

      wg
        .removeAll()
        .add(menu.wg)
        .add(body)
      ;
    });
  }

  static function show (wg: Domo, year: String, all: All): Void {
    var y = all.data[year];
    if (y == null) {
      Ui.alert(_args(_("Year %0 not found"), [year]));
      y = all.lastYear();
    }

    final d = y.treasury();
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Summary")))
      .add( Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right; width: 80px")
            .text(_("Profits") + ":"))
          .add(Q("td")
            .klass("number")
            .text(Dec.toIso(d.summary, 2)))))
      .add(Q("div")
        .klass("head")
        .text(_("Annotations")))
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Nick")))
          .add(Q("td")
            .klass("header")
            .text(_("Stocks")))
          .add(Q("td")
            .klass("header")
            .text(_("Sold")))
          .add(Q("td")
            .klass("header")
            .text(_("Bought")))
          .add(Q("td")
            .klass("header")
            .text(_("Profits"))))
        .adds(d.entries.map(e ->
            Q("tr")
              .add(Q("td")
                .text(e.nick))
              .add(Q("td")
                .klass("number2")
                .text(Dec.toIso(e.stocks, 0)))
              .add(Q("td")
                .klass("number")
                .text(Dec.toIso(e.total, 2)))
              .add(Q("td")
                .klass("number")
                .text(Dec.toIso(e.total - Opt.eget(e.profits), 2)))
              .add(Q("td")
                .klass("number")
                .text(Dec.toIso(Opt.eget(e.profits), 2)))
          ))
      )
    ;
  }
}

