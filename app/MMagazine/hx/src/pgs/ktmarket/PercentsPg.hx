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
import data.IbexSundays;
import dm.LineChart;
import I18n._;

/// Profits page.
class PercentsPg {
  static final pfColor = "#000000";
  static final ibexColor = "#800000";
  final wg: Domo;
  final invs: Int;
  final initialAssets: Array<Float>;
  final profits: Array<ProfitsEntry>;
  final ibexdts: Array<Date>;
  final ibexrts: Array<Float>;

  function new (
    wg: Domo, investors: Int, initialAssets: Array<Float>,
    profits: Array<ProfitsEntry>, ibex: IbexSundays
  ) {
    if (profits.length == 0) {
      profits = [new ProfitsEntry(
        Dt.to(Dt.mk(1, 1, Dt.year(Date.now()))), [0, 0, 0]
      )];
    }
    if (profits.length == 1) {
      profits.push(profits[0]);
    }

    var ibexdts = ibex.dates();
    var ibexrts = ibex.ratios();
    if (ibexdts.length == 0) {
      ibexdts.push(Date.now());
      ibexrts.push(0.0);
    }
    if (ibexdts.length == 1) {
      ibexdts.push(ibexdts[0]);
      ibexrts.push(0.0);
    }
    this.ibexdts = ibexdts;
    this.ibexrts = ibexrts;

    this.wg = wg;
    this.invs = investors;
    this.initialAssets = initialAssets;
    this.profits = profits;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    if (profits[0].profits.length != invs) {
      throw new haxe.Exception("Investor number is not " + invs);
    }

    if (!It.from(profits).map(p -> p.date).eq(
      It.from(ibexdts).map(d -> Dt.to(d)), (p, i) -> p == i
    )){
      badData();
      return;
    }

    var sumInitialAssets = 0.0;
    var sumInitialPfs = 0.0;
    final initialPfs = [];
    final sums = [];
    final labels = [];
    final sets = [];
    for (i in 0...invs) {
      final pfs = profits[0].profits[i];
      initialPfs.push(pfs);
      sumInitialPfs += pfs;
      sumInitialAssets += initialAssets[i];
      sets.push([]);
    }
    for (e in profits) {
      labels.push(e.date.substring(6) + "/" + e.date.substring(4, 6));
      final pfs = e.profits;
      var sum = 0.0;
      for (i in 0...invs) {
        final dif = pfs[i] - initialPfs[i];
        sets[i].push(Some(dif * 100 / initialAssets[i]));
        sum += dif;
      }
      sums.push(Some(sum * 100 / sumInitialAssets));
    }
    sets.push(ibexrts.map(v -> Some(v * 100)));
    sets.push(sums);

    final setAtts = [];
    for (i in 0...invs) {
      final atts = LineChartLine.mk();
      atts.color = Cts.toSellColors[i];
      setAtts.push(atts);
    }
    final ibexAtts = LineChartLine.mk();
    ibexAtts.color = ibexColor;
    ibexAtts.width = 3;
    setAtts.push(ibexAtts);
    final pfAtts = LineChartLine.mk();
    pfAtts.color = pfColor;
    pfAtts.width = 3;
    setAtts.push(pfAtts);

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

    final data2 = new LineChartData(
      labels,
      [It.range(sums.length).map(i ->
        Opt.bind(sums[i], sum -> Some(sum - ibexrts[i] * 100))
      ).to()],
      [pfAtts]
    );
    data2.drawLabel = data.drawLabel;
    data2.drawGrid = data.drawGrid;

    final chart2 = LineChart.mk();
    chart2.exArea.width = 600;
    chart2.exArea.height = 200;
    chart2.inPadding = new LineChartPadding(25, 25, 30, 100);
    chart2.data = data2;

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .add(caption())))
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;")
            .text("%"))
          .add(Q("td")
            .add(chart.mkWg())))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "2")
            .style("height:15px")
            .text(" ")))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .add(caption2())))
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;")
            .text("%"))
          .add(Q("td")
            .add(chart2.mkWg())))
        )
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
              .style("vertical-align: middle;text-align:left;")
              .text(_("Investor") + "-" + i))
        ).to())
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(Ui.led(ibexColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text("ibex")))
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(Ui.led(pfColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text(_("Investors average"))))
    ;
  }

  function caption2 () {
    return Q("table")
      .klass("frame")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(Ui.led(pfColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text(_("Investors - Ibex"))))
    ;
  }

  function badData () {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .klass("frame")
            .text(_("Dates of profits and ibex does not match"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center;width:50%")
            .klass("border")
            .text(_("Profits")))
          .add(Q("td")
            .style("text-align: center")
            .klass("border")
            .text(_("Ibex"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center; vertical-align:top")
            .html("<tt>" +
                It.join(It.from(profits).map(p -> p.date), "<br>") +
                "<tt>"
              ))
          .add(Q("td")
            .style("text-align:center; vertical-align:top")
            .html("<tt>" +
                It.join(It.from(ibexdts).map(d -> Dt.to(d)), "<br>") +
                "<tt>"
              ))))
    ;
  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("KtMarket"),
      "source" => Js.ws("PercentsPg"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final investors = rp["investors"].ri();
      final initialAssets = rp["initialAssets"].ra().map(e -> e.rf());
      final profits = rp["profits"].ra().map(e -> ProfitsEntry.fromJs(e));
      final ibex = IbexSundays.fromJs(rp["ibex"]);

      new PercentsPg(wg, investors, initialAssets, profits, ibex).show();
    });
  }


}
