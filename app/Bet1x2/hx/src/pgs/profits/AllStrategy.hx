// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.profits;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Dec;
import data.Profits;
import I18n._;

/// AllStrategy subpage.
class AllStrategy {
  final wg: Domo;
  final strategies: Array<Profits>;
  final sum: Profits;

  function new (wg: Domo, strategies: Array<Profits>, sum: Profits) {
    strategies.sort((s1, s2) -> s1.description > s2.description ? 1 : -1);
    this.wg = wg;
    this.strategies = strategies;
    this.sum = sum;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final sumBets = sum.fails + sum.hits;
    wg
      .removeAll()
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
          .text(sum.description))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(sum.hits, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(sumBets > 0 ? Dec.toIso(sum.hits * 100 / sumBets, 2) : "--"))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(sum.fails, 0)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(sumBets > 0
              ? Dec.toIso(100 - (sum.hits * 100 / sumBets), 2)
              : "--"
            ))
          .add(Q("td").klass("pre"))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(Dec.toIso(sum.amount, 2)))
          .add(Q("td")
          .klass("pre")
          .style("text-align:right")
          .text(sumBets > 0 ? Dec.toIso(sum.amount * 100 / sumBets, 2) : "--")))
        .add(Q("tr")
          .add(Q("td")
            .klass("pre")
            .att("colspan", "10")
            .add(Q("hr"))))
        .adds(It.from(strategies).map(s -> {
            final bets = s.fails + s.hits;
            return Q("tr")
              .add(Q("td")
              .klass("pre")
              .style("text-align:left")
              .text(s.description))
              .add(Q("td").klass("pre"))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(Dec.toIso(s.hits, 0)))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(bets > 0 ? Dec.toIso(s.hits * 100 / bets, 2) : "--"))
              .add(Q("td").klass("pre"))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(Dec.toIso(s.fails, 0)))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(bets > 0 ? Dec.toIso(100 - (s.hits * 100 / bets), 2) : "--"))
              .add(Q("td").klass("pre"))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(Dec.toIso(s.amount, 2)))
              .add(Q("td")
              .klass("pre")
              .style("text-align:right")
              .text(bets > 0 ? Dec.toIso(s.amount * 100 / bets, 2) : "--"))
            ;
          }).to()))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, strategy: String): Void {
    Cts.client.send([
      "source" => Js.ws("AllStrategy"),
      "rq" => Js.ws("idata"),
      "strategy" => Js.ws(strategy)
    ], rp -> {
      final strategies = rp["strategies"].ra().map(e -> Profits.fromJs(e));
      final sum = Profits.fromJs(rp["sum"]);
      new AllStrategy(wg, strategies, sum).show();
    });
  }

}
