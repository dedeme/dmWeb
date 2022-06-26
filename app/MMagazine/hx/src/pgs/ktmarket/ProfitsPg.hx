// Copyright 04-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.ktmarket;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dt;
import dm.It;
import dm.Opt;
import data.ProfitsEntry;
import dm.LineChart;
import I18n._;

/// Profits page.
class ProfitsPg {
  final invs: Int;
  final wg: Domo;
  final profits: Array<ProfitsEntry>;

  function new (wg: Domo, investors: Int, profits: Array<ProfitsEntry>) {
    if (profits.length == 0) {
      profits = [new ProfitsEntry(
        Dt.to(Dt.mk(1, 1, Dt.year(Date.now()))), [0, 0, 0]
      )];
    }
    if (profits.length == 1) {
      profits.push(profits[0]);
    }
    this.wg = wg;
    this.invs = investors;
    this.profits = profits;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    if (profits[0].profits.length != invs) {
      throw new haxe.Exception("Investor number is not " + invs);
    }
    final labels = [];
    final sets = [];
    for (i in 0...invs) sets.push([]);
    for (e in profits) {
      labels.push(e.date.substring(6) + "/" + e.date.substring(4, 6));
      final pfs = e.profits;
      for (i in 0...invs) {
        sets[i].push(Some(pfs[i]));
      }
    }
    final setAtts = [
      LineChartLine.mk(),
      LineChartLine.mk(),
      LineChartLine.mk()
    ];
    for (i in 0...invs) setAtts[i].color = Cts.toSellColors[i];
    final data = new LineChartData(labels, sets, setAtts);
    final lenGroup = Std.int(labels.length / 10) + 1;
    data.drawLabel = (l, i) -> i > 0 && i % lenGroup == 0;
    data.drawGrid = (l, i) ->
      i > 0 && i % lenGroup == 0 && i < labels.length - 1;

    final chart = LineChart.mk();
    chart.exArea.width = 600;
    chart.exArea.height = 400;
    chart.inPadding = new LineChartPadding(25, 25, 30, 100);
    chart.data = data;

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(caption())))
        .add(Q("tr")
          .add(Q("td")
            .add(chart.mkWg()))))
    ;
  }

  function caption () {
    return Q("table")
      .klass("frame")
      .att("align", "center")
      .adds(It.range(invs).map(i ->
          Q("tr")
            .add(Q("td")
              .style("width:30px")
              .add(Ui.led(Cts.toSellColors[i], 6)))
            .add(Q("td")
              .style("vertical-align: middle")
              .text(_("Investor") + "-" + i))
        ).to())
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("KtMarket"),
      "source" => Js.ws("ProfitsPg"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final investors = rp["investors"].ri();
      final profits = rp["profits"].ra().map(e -> ProfitsEntry.fromJs(e));

      new ProfitsPg(wg, investors, profits).show();
    });
  }


}
