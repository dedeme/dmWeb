// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.It;
import dm.Dec;
import data.Market;
import pgs.wgs.ResultsChart;
import I18n._;

/// Savings page.
class SavingsPg {
  final wg: Domo;
  final market: Market;
  final lenDates: Int;

  public function new (wg: Domo, market: Market) {
    this.wg = wg;
    this.market = market;
    lenDates = market.dates.length;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final rss = market.results;
    rss.sort((r1, r2) ->
      r1.model == "CM" ? -11 : r2.model == "CM" ? 1
        : dm.Str.compare(r1.model, r2.model)
    );
    final trs: Array<Domo> = [];
    for (rs in rss) {
      final md = Opt.eget(It.from(market.models).find(m -> m.id == rs.model));
      final ibexRs = Opt.eget(It.from(rs.results).find(
        r -> r.nick == "IBEX"
      )).propResults.results.map(r -> r.savings);
      final coRs = It.from(rs.results).filter(r -> r.nick != "IBEX").to();
      final pondRs: Array<Float> = [];
      final propRs: Array<Float> = [];
      for (i in 0...lenDates) {
        pondRs.push(It.from(coRs).reduce(0.0, (seed, co) ->
          seed + co.pondResults.results[i].savings
        ));
        propRs.push(It.from(coRs).reduce(0.0, (seed, co) ->
          seed + co.propResults.results[i].savings
        ));
      }
      trs.push(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("div")
            .klass("head")
            .html(md.name + (md.params.length > 0 ? "<br>" : "") +
              md.params.map(p -> Dec.toIso(p * 100, 2) + "%")
                .join(", "))))
      );
      final trs2: Array<Domo> = [];
      trs2.push(Q("tr")
        .add(Q("td")
          .add(Ui.led(Cts.ibexColor)))
        .add(Q("td")
          .text("IBEX"))
        .add(Q("td")
          .text(Dec.toIso(ibexRs[lenDates - 1], 2)))
      );
      trs2.push(Q("tr")
        .add(Q("td")
          .add(Ui.led(Cts.pondColor)))
        .add(Q("td")
          .text("POND"))
        .add(Q("td")
          .text(Dec.toIso(pondRs[lenDates - 1], 2)))
      );
      trs2.push(Q("tr")
        .add(Q("td")
          .add(Ui.led(Cts.propColor)))
        .add(Q("td")
          .text("PROP"))
        .add(Q("td")
          .text(Dec.toIso(propRs[lenDates - 1], 2)))
      );
      trs.push(Q("tr")
        .add(Q("td")
          .style("width:5px;vertical-align:middle;")
          .add(Q("table")
            .adds(trs2)))
        .add(Q("td")
          .add(new ResultsChart(
              market.dates,
              ibexRs,
              pondRs,
              propRs
            ).wg)
      ));
    }
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .adds(trs))
    ;

  }
}
