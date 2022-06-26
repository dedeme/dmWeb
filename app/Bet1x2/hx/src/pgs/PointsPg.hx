// Copyright 26-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dt;
import dm.Opt;
import dm.Menu;
import data.Result;
import wgs.TeamTable;
import I18n._;

/// Points page.
class PointsPg {
  final wg: Domo;
  var years: Array<String>;
  var teams: Array<Array<String>>; // Array<[id, name]>
  var points: Array<Array<Option<Result>>>;
  var year: String;

  function new (
    wg: Domo,
    years: Array<String>,
    teams: Array<Array<String>>,
    points: Array<Array<Option<Result>>>
  ) {
    years.sort((e1, e2) -> e1 < e2 ? 1 : -1);
    year = years[0];
    this.wg = wg;
    this.years = years;
    this.teams = teams;
    this.points = points;
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
    new TeamTable(rstb, teams, points, r -> "" + r.home + "-" + r.out).show();

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
      "source" => Js.ws("PointsPg"),
      "rq" => Js.ws("yearData"),
      "year" => Js.ws(y)
    ], rp -> {
      teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      points = rp["points"].ra().map(row ->
        row.ra().map(e -> e.isNull() ? None : Some(Result.fromJs(e)))
      );
      year = y;

      show();
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("PointsPg"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(e -> e.rs());
      final teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      final points = rp["points"].ra().map(row ->
        row.ra().map(e -> e.isNull() ? None : Some(Result.fromJs(e)))
      );

      new PointsPg(wg, years, teams, points).show();
    });
  }

}
