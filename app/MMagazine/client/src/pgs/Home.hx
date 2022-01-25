// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import cm.data.RankingEntry;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;
  final bestRanking: RankingEntry;
  final isNewBestRanking: Bool;

  function new (
    wg: Domo,
    bestRanking: RankingEntry, isNewBestRanking: Bool
  ) {
    this.wg = wg;
    this.bestRanking = bestRanking;
    this.isNewBestRanking = isNewBestRanking;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .adds(It.range(2).map(i -> Q("div").klass("separator")).to())
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("span")
              .text(
                "· " +
                (isNewBestRanking
                  ? _("There is a new best parameter")
                  : _("This week continue the same best parameter")) +
                ": " + Dec.toIso(bestRanking.param / 10000, 1) +
                " " + _("with") + " " + Dec.toIso(bestRanking.value * 1000, 2) +
                " " + _("points") + ". "))
            .add(Q("a")
              .att("href", "?rankings")
              .klass("link")
              .text(_("[ See Rankings page ]"))))))
    ;
  }

  // Control -------------------------------------------------------------------

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final bestRanking = RankingEntry.fromJs(rp["bestRanking"]);
      final isNewBestRanking = rp["isNewBestRanking"].rb();
      new Home(wg, bestRanking, isNewBestRanking).show();
    });
  }
}
