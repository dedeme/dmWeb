// Copyright 26-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dt;
import dm.Dec;
import dm.Opt;
import dm.Menu;
import data.Bet;
import wgs.TeamTable;
import I18n._;

/// Bets page.
class BetsPg {
  final wg: Domo;
  var years: Array<String>;
  var teams: Array<Array<String>>; // Array<[id, name]>
  var bets: Array<Array<Option<Bet>>>;
  var year: String;

  function new (
    wg: Domo,
    years: Array<String>,
    teams: Array<Array<String>>,
    bets: Array<Array<Option<Bet>>>
  ) {
    years.sort((e1, e2) -> e1 < e2 ? 1 : -1);
    year = years[0];
    this.wg = wg;
    this.years = years;
    this.teams = teams;
    this.bets = bets;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final y = years[0];
    final lopts = [Menu.toption(y, y, () -> sel(y))];
    for (i in 1...years.length) {
      final y = years[i];
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(y, y, () -> sel(y)));
    }
    final menu = new Menu(lopts, [], year);

    final rstb = Q("div");
    new TeamTable(rstb, teams, bets,
      b ->
        Dec.toIso(b.r1, 2) + "-" + Dec.toIso(b.rx, 2) + "-" +Dec.toIso(b.r2, 2)
    ).show();

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(_("Points")))
      .add(rstb)
    ;
  }

  // Control -------------------------------------------------------------------

  function sel (y: String) {
    Cts.client.send([
      "source" => Js.ws("BetsPg"),
      "rq" => Js.ws("yearData"),
      "year" => Js.ws(y)
    ], rp -> {
      teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      bets = rp["bets"].ra().map(row ->
        row.ra()
          .map(e -> e.ra())
          .map(e -> e.length == 0 ? None : Some(Bet.fromJs(e[0])))
      );
      year = y;

      show();
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("BetsPg"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(e -> e.rs());
      final teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      final bets = rp["bets"].ra().map(row ->
        row.ra()
          .map(e -> e.ra())
          .map(e -> e.length == 0 ? None : Some(Bet.fromJs(e[0])))
      );

      new BetsPg(wg, years, teams, bets).show();
    });
  }

}
