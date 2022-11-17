// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.It;
import dm.Dec;
import dm.Str;
import dm.Vmenu;
import dm.Tp;
import data.Market;
import data.CoChart;
import pgs.wgs.CoChartWg;
import I18n._;

/// References page.
class RefsPg {
  final wg: Domo;
  final market: Market;
  final lenDates: Int;
  final models: Array<String>;
  var selModel: String;

  public function new (wg: Domo, market: Market) {
    this.wg = wg;
    this.market = market;
    lenDates = market.dates.length;

    models = It.from(market.models).filter(m -> m.id != "CM")
      .map(m -> m.id)
      .sort(Str.compare)
      .to();

    selModel = models[0];
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final ops = [Vmenu.title(_("Models"))];
    for (m in models) {
      ops.push(Vmenu.option(m, m, () -> select(m)));
    }
    final vmenu = new Vmenu(ops, selModel);

    final dates = market.dates;
    final rs = Opt.eget(It.from(market.results).find(r -> r.model == selModel));
    var ibex: CoChart;
    final cos: Array<CoChart> = [];

    for (nkRs in rs.results) {
      final data = new CoChart(
        nkRs.nick,
        nkRs.pondResults.results[lenDates - 1].assets,
        nkRs.propResults.results[lenDates - 1].assets,
        It.from(nkRs.pondResults.orders).filter(o -> o.bs == "s").count(),
        market.dates,
        nkRs.closes,
        nkRs.refs
      );

      if (nkRs.nick == "IBEX") ibex = data;
      else cos.push(data);
    }
    cos.sort((c1, c2) -> return Str.compare(c1.nick, c2.nick));

    final trs = [Q("tr")
      .add(Q("td"))
      .add(Q("td").add(new CoChartWg(ibex).wg))
      .add(Q("td"))
    ];

    var i = 0;
    final len = cos.length;
    while (true) {
      if (i >= len) break;
      trs.push(Q("tr")
        .add(Q("td").add(new CoChartWg(cos[i]).wg))
        .add(i + 1 < len
          ? Q("td").add(new CoChartWg(cos[i + 1]).wg)
          : Q("td")
        ).add(i + 2 < len
          ? Q("td").add(new CoChartWg(cos[i + 2]).wg)
          : Q("td")
        )
      );

      i += 3;
    }

    wg
      .removeAll()
      .add(Q("table")
      .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top;")
            .add(vmenu.wg))
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .adds(trs)))))
    ;

  }

  // Control -------------------------------------------------------------------

  function select (md: String): Void {
    selModel = md;
    show();
  }
}
