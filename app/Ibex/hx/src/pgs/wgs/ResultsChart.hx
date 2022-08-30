// Copyright 28-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.LineChart;
import dm.Opt;

class ResultsChart {
  public final wg: Domo;
  public function new (
    dates: Array<String>,
    ibex: Array<Float>,
    pond: Array<Float>,
    prop: Array<Float>
  ) {
    final chart = LineChart.mk();
    chart.exArea.width = 500;
    chart.exArea.height = 200;
    chart.inPadding.left = 85;

    final ds = dates.map(e -> e.substring(4, 6));
    final ibexAtts = LineChartLine.mk();
    ibexAtts.color = Cts.ibexColor;
    final pondAtts = LineChartLine.mk();
    pondAtts.color = Cts.pondColor;
    final propAtts = LineChartLine.mk();
    propAtts.color = Cts.propColor;
    chart.data = new LineChartData(
      ds,
      [ ibex.map(v -> Some(v)),
        pond.map(v -> Some(v)),
        prop.map(v -> Some(v))
      ],
      [ ibexAtts,
        pondAtts,
        propAtts
      ]
    );
    chart.data.drawLabel = (s, i) ->
      (s == "01" || s == "04" || s == "07" || s == "10")
      && i != 0 && ds[i] != ds[i-1];
    chart.data.drawGrid = (s, i) ->
      (s == "01" || s == "04" || s == "07" || s == "10")
      && i != 0 && ds[i] != ds[i-1];

    wg = chart.mkWg();
  }
}
