// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dt;
import dm.Opt;
import dm.Tp;
import dm.Menu;
import dm.Vmenu;
import data.Result;
import pgs.profits.AllTotal;
import pgs.profits.AllStrategy;
import pgs.profits.YearTotal;
import pgs.profits.YearStrategy;
import I18n._;

/// Home page.
class Home {
  final wg: Domo;
  final years: Array<String>;
  final strategies: Array<String>;
  final doc: Map<String, String>;
  var year: String;
  var strategy: String;

  public function new (
    wg: Domo,
    years: Array<String>,
    doc: Map<String, String>
  ) {
    years.sort((e1, e2) -> e1 < e2 ? 1 : -1);
    year = "all";
    strategies = It.fromMap(doc).map(tp -> tp.e1).to();
    strategies.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    strategy = "total";
    this.wg = wg;
    this.years = years;
    this.doc = doc;

  }

  // View ----------------------------------------------------------------------

  public function show () {
    final lopts = [
      Menu.toption("all", _("All"), () -> selMainMenu("all")),
      Menu.separator2(),
      Menu.toption(years[0], years[0], () -> selMainMenu(years[0]))
    ];
    for (i in 1...years.length) {
      final y = years[i];
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(y, y, () -> selMainMenu(y)));
    }
    final menu = new Menu(lopts, [], year);

    final vopts = [
      Vmenu.title(_("Strategies")),
      Vmenu.separator(),
      Vmenu.option("total", _("Total"), () -> selVmenu("total")),
      Vmenu.separator(),
      Vmenu.option(strategies[0], strategies[0], () -> selVmenu(strategies[0]))
    ];
    for (i in 1...strategies.length) {
      final s = strategies[i];
      vopts.push(Vmenu.option(s, s, () -> selVmenu(s)));
    }
    final vmenu = new Vmenu(vopts, strategy);

    final body = Q("div");

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width: 5px;vertical-align:top")
            .add(vmenu.wg))
          .add(body)))
    ;

    if (year == "all") {
      if (strategy == "total") AllTotal.mk(body);
      else AllStrategy.mk(body, strategy);
    } else {
      if (strategy == "total") YearTotal.mk(body, year);
      else YearStrategy.mk(body, year, strategy);
    }

  }

  // Control -------------------------------------------------------------------

  function selMainMenu (y: String): Void {
    year = y;
    show();
  }

  function selVmenu (s: String): Void {
    strategy = s;
    show();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(e -> e.rs());
      final doc = Opt.get(It.fromMap(rp["doc"].ro()).map(tp ->
        new Tp(tp.e1, tp.e2.rs())
      ).toMap());

      new Home(wg, years, doc).show();
    });
  }

}
