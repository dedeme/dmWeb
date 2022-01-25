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

/// AllTotal subpage.
class AllTotal {
  final wg: Domo;
  final strategies: Array<Profits>;

  function new (wg: Domo, strategies: Array<Profits>) {
    strategies.sort((s1, s2) -> s1.description > s2.description ? 1 : -1);
    this.wg = wg;
    this.strategies = strategies;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
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

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("AllTotal"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final strategies = rp["strategies"].ra().map(e -> Profits.fromJs(e));
      new AllTotal(wg, strategies).show();
    });
  }

}
