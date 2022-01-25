// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.profits;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Opt;
import dm.Dec;
import data.Decision;
import data.Profits;
import wgs.TeamTable;
import I18n._;

/// YearStrategy subpage.
class YearStrategy {
  final wg: Domo;
  final teams: Array<Array<String>>; // Array<[id, name]>
  final decisions: Array<Array<Option<Decision>>>;
  final profits: Profits;

  function new (
    wg: Domo,
    teams: Array<Array<String>>,
    decisions: Array<Array<Option<Decision>>>,
    profits: Profits
  ) {
    this.wg = wg;
    this.teams = teams;
    this.decisions = decisions;
    this.profits = profits;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final bets = profits.hits + profits.fails;

    var hits = 0;
    var fails = 0;
    var amount = 0.0;
    for (row in decisions) {
      for (dec in row) {
        switch (dec) {
          case Some(d):
            if (d.isOk()) ++hits; else ++fails;
            amount += d.profits();
          case None:
        }
      }
    }
    final prf2 = new Profits(profits.description, hits, fails, amount);
    final bets2 = prf2.hits + prf2.fails;

    final dectb = Q("div");
    new TeamTable(
      dectb,
      teams,
      decisions,
      d -> "" + d.result.home + "-" + d.result.out + "<br>" +
        d.points.home + "-" + d.points.out + "<br>" +
        Dec.toIso(d.bet.r1, 2) + "-" + Dec.toIso(d.bet.rx, 2) + "-" +
          Dec.toIso(d.bet.r2, 2) + "<br>" +
        (d.isOk() ? "[+] " : "[-] ") +
          ( switch (d.decision) {
              case CtsBet_1: "1";
              case CtsBet_x: "x";
              default: "2";
            })
    ).show();

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("From Server")))
      .add(Q("table")
        .style("border-collapse : collapse;")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("th")
            .text(_("Description")))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Hits")))
          .add(Q("th")
            .text("%"))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Fails")))
          .add(Q("th")
            .text("%"))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Profits")))
          .add(Q("th")
            .text("%")))
        .add(Q("tr")
          .add(Q("td")
          .klass("pre")
          .style("text-align:left")
          .text(profits.description))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(profits.hits, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets > 0 ? Dec.toIso(profits.hits * 100 / bets, 2) : "--"))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(profits.fails, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets > 0
              ? Dec.toIso(100 - (profits.hits * 100 / bets), 2)
              : "--"
            ))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(profits.amount, 2)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets > 0 ? Dec.toIso(profits.amount * 100 / bets, 2) : "--"))))
      .add(Q("div")
        .klass("head")
        .text(_("From Client")))
      .add(Q("table")
        .style("border-collapse : collapse;")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("th")
            .text(_("Description")))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Hits")))
          .add(Q("th")
            .text("%"))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Fails")))
          .add(Q("th")
            .text("%"))
          .add(Q("th"))
          .add(Q("th")
            .text(_("Profits")))
          .add(Q("th")
            .text("%")))
        .add(Q("tr")
          .add(Q("td")
          .klass("pre")
          .style("text-align:left")
          .text(prf2.description))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(prf2.hits, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets2 > 0 ? Dec.toIso(prf2.hits * 100 / bets2, 2) : "--"))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(prf2.fails, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets2 > 0
              ? Dec.toIso(100 - (prf2.hits * 100 / bets2), 2)
              : "--"
            ))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(prf2.amount, 2)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(bets2 > 0 ? Dec.toIso(prf2.amount * 100 / bets2, 2) : "--"))))
      .add(Q("div")
        .klass("head")
        .text(_("Decisions")))
      .add(dectb)
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, year: String, strategy: String): Void {
    Cts.client.send([
      "source" => Js.ws("YearStrategy"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(year),
      "strategy" => Js.ws(strategy)
    ], rp -> {
      final teams = rp["teams"].ra().map(t -> t.ra().map(e -> e.rs()));
      final decisions = rp["decisions"].ra().map(row ->
        row.ra().map(e -> e.isNull() ? None : Some(Decision.fromJs(e)))
      );
      final profits = Profits.fromJs(rp["profits"]);
      new YearStrategy(wg, teams, decisions, profits).show();
    });
  }

}
