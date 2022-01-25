// Copyright 21-Dic-2021 ºDeme
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

/// Standings page.
class Standings {
  final wg: Domo;
  var years: Array<String>;
  var teams: Array<Array<String>>; // Array<[id, name]>
  var results: Array<Array<Option<Result>>>;
  var year: String;

  function new (
    wg: Domo,
    years: Array<String>,
    teams: Array<Array<String>>,
    results: Array<Array<Option<Result>>>
  ) {
    years.sort((e1, e2) -> e1 < e2 ? 1 : -1);
    year = years[0];
    this.wg = wg;
    this.years = years;
    this.teams = teams;
    this.results = results;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final lopts = [Menu.toption(year, year, () -> sel(year))];
    for (i in 1...years.length) {
      final y = years[i];
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(y, y, () -> sel(y)));
    }
    final menu = new Menu(lopts, [], year);

    final rstb = Q("div");
    new TeamTable(rstb, teams, results, r -> "" + r.home + "-" + r.out).show();

    final pttb = Q("table")
      .att("align", "center")
      .style("border-collapse: collapse;")
      .adds(It.range(teams.length).map(row -> Q("tr")
          .add(Q("td")
            .klass("border")
            .style("white-space:nowrap;")
            .add(Q("span")
              .add(Ui.img("clubs/" + teams[row][0])
              .klass("badge")
              .style("vertical-align:middle")))
            .add(Q("span")
              .text(teams[row][1])))
          .add(Q("td")
            .klass("pre")
            .style("text-align: right")
            .text("" + It.range(teams.length).reduce(0, (s, i) ->
                s +
                (switch (results[row][i]) {
                  case Some(r): r.homePoints();
                  case None: 0;
                }) +
                (switch (results[i][row]) {
                  case Some(r): r.outPoints();
                  case None: 0;
                })
              )))
        ).to())
    ;

    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div")
        .klass("head")
        .text(_("Results")))
      .add(rstb)
      .add(Q("div")
        .klass("head")
        .text(_("Points")))
      .add(pttb)
    ;
  }

  // Control -------------------------------------------------------------------

  function sel (y: String) {
    Cts.client.send([
      "source" => Js.ws("Standings"),
      "rq" => Js.ws("yearData"),
      "year" => Js.ws(y)
    ], rp -> {
      teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      results = rp["results"].ra().map(row ->
        row.ra().map(e -> e.isNull() ? None : Some(Result.fromJs(e)))
      );
      year = y;

      show();
    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.send([
      "source" => Js.ws("Standings"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(e -> e.rs());
      final teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      final results = rp["results"].ra().map(row ->
        row.ra().map(e -> e.isNull() ? None : Some(Result.fromJs(e)))
      );

      new Standings(wg, years, teams, results).show();
    });
  }

}
