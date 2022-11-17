// Copyright 28-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.LineChart;
import dm.Opt;
import dm.Dec;
import dm.It;
import data.CoChart;

class CoChartWg {
  public final wg = Q("div");

  public function new (data: CoChart) {
    final len = data.dates.length;

    final chart = LineChart.mk();
    chart.exArea.width = 300;
    chart.exArea.height = 150;
    chart.inPadding.left = data.nick == "IBEX" ? 85 : 50;

    final ds = data.dates.map(e -> e.substring(4, 6));
    final closesAtts = LineChartLine.mk();
    closesAtts.color = Cts.closesColor;
    final boughtAtts = LineChartLine.mk();
    boughtAtts.color = Cts.boughtColor;
    final soldAtts = LineChartLine.mk();
    soldAtts.color = Cts.soldColor;
    chart.data = new LineChartData(
      ds,
      [ data.closes.map(v -> Some(v)),
        It.range(len).map(i ->
          data.refs[i] < data.closes[i] ? Some(data.refs[i]): None).to(),
        It.range(len).map(i ->
          data.refs[i] > data.closes[i] ? Some(data.refs[i]): None).to()
      ],
      [ closesAtts,
        boughtAtts,
        soldAtts
      ]
    );
    chart.data.drawLabel = (s, i) ->
      (s == "01" || s == "05" || s == "09")
      && i != 0 && ds[i] != ds[i-1];
    chart.data.drawGrid = (s, i) ->
      (s == "01" || s == "05" || s == "09")
      && i != 0 && ds[i] != ds[i-1];

    wg
      .add(Q("table")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .text("Po: " + Dec.toIso(data.pond, 2)))
          .add(Q("td")
            .style("text-align:center")
            .text(data.nick + "(" + data.sales + ")"))
          .add(Q("td")
            .style("text-align:right")
            .text("Pr: " + Dec.toIso(data.prop, 2))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(chart.mkWg()))))
    ;
  }
}
