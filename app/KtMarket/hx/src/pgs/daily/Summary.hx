// Copyright 21-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Store;
import dm.Opt;
import dm.It;
import dm.Menu;
import dm.Clock;
import dm.LineChart;
import data.DailyChart;
import data.IxsChartEntry;
import I18n._;

/// Daily charts summary
class Summary {
  static var key = "MultiMarket_chart_manager";

  final wg: Domo;
  final data: Array<DailyChart>;
  final ixsData: Array<IxsChartEntry>;
  final capitals: Array<Float>;
  var mSel: Int;

  /// Constructor.
  ///   wg      : Container.
  ///   data    : All the comapanies data.
  ///   ixsData : Indexes data.
  ///   capitals: Investor capitals.
  public function new (
    wg: Domo, data: Array<DailyChart>,
    ixsData: Array<IxsChartEntry>, capitals: Array<Float>
  ) {
    this.wg = wg;
    this.data = data;
    this.ixsData = ixsData;
    this.capitals = capitals;
    switch Store.get(key) {
      case Some(sel): mSel = Std.parseInt(sel);
      case None: mSel = -1;
    }

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final sel = mSel == -1 ? "All" : _("Inv-") + Std.string(mSel);
    final lopts = [
      Menu.toption("All", _("All"), () -> setMenu(-1))
    ];
    for (i in 0...data[0].investorsData.length) {
      final op = _("Inv-") + Std.string(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () -> setMenu(i)));
    }
    final ropts = [];
    final menu = new Menu(lopts, ropts, sel);

    final iCo = mSel;
    final hours = data[0].hours;
    var investing = 0.0;
    var yesterdayProfits = 0.0;
    var todayProfits = 0.0;
    final profits = hours.map(e -> 0.0);
    for (co in data) {
      var inv = 0.0;
      var yprof = 0.0;
      var prof = 0.0;
      if (iCo == -1) {
        for (acc in co.investorsData) {
          if (acc.stocks > 0) {
            final iv = acc.stocks * acc.price;
            inv += iv;
            if (!acc.todayBuy) {
              yprof += acc.stocks * co.close - iv;
            }
            final q = Fns.validQuote(
              co.quotes, co.quotes.length - 1, acc.price
            );
            prof += acc.stocks * q - iv;
            for (i in 0...profits.length) {
              final q = Fns.validQuote(co.quotes, i, acc.price);
              profits[i] += acc.stocks * q - iv;
            }
          }
        }
      } else {
        final acc = co.investorsData[iCo];
        if (acc.stocks > 0) {
          final iv = acc.stocks * acc.price;
          inv = iv;
          if (!acc.todayBuy) {
            yprof = acc.stocks * co.close - iv;
          }
          final q = Fns.validQuote(co.quotes, co.quotes.length - 1, acc.price);
          prof = acc.stocks * q - iv;
          for (i in 0...profits.length) {
            final q = Fns.validQuote(co.quotes, i, acc.price);
            profits[i] += acc.stocks * q - iv;
          }
        }
      }

      investing += inv;
      yesterdayProfits += yprof;
      todayProfits += prof;
    }

    final dailyProfits = todayProfits - yesterdayProfits;
    final initialValue = investing + yesterdayProfits;
    final ratio = initialValue > 0 ? dailyProfits / initialValue : 0;
    wg
      .removeAll()
      .add(menu.wg)
      .add(Q("div").style("text-align:center;")
        .add(Q("div")
          .klass("head")
          .style("padding-bottom:8px")
          .html(_("Summary")))
        .add(Q("div")
          .add(Q("span")
            .klass("frame")
            .style(
              'font-size:x-large;color:' +
              '${ratio > 0 ? "#00AAFF" : ratio < 0 ? "#FF8100" : "#000000"}'
            )
            .html(
              ' ${Dec.toIso(ratio * 100, 2)}% |' +
              ' ${Dec.toIso(dailyProfits, 2)}€ '
            )))
        .add(ChBig.mk(hours, profits, ratio))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("text-align:left")
              .add(ixsText(0)))
            .add(Q("td")
              .style("text-align:right")
              .add(ixsText(1))))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .add(ixsChart()))))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .style("vertical-align:bottom")
              .add(new Clock().wg
                .klass("frame")
                .style(
                  "background:radial-gradient(#000333,#e6f6f6);" +
                  "margin-top: 8px;"
                )))
            .add(Q("td")
              .style("vertical-align:bottom")
              .add(Q("iframe")
                .klass("frame")
                .att("width", "450px")
                .att("height", "133px")
                .att(
                  "src",
                  "http://www.aemet.es/es/eltiempo/prediccion/" +
                  "municipios/mostrarwidget/rivas-vaciamadrid-id28123?" +
                  "w=g4p01110001ohmffffffw450z133x4f86d9t95b6e9r0s4n1"
                ))))))
    ;
  }

  function ixsText (index: Int): Domo {
    final text = index == 0 ? "IBEX" : "EUROSTOXX";
    final txColor = index == 0 ? "#000080" : "#008000";
    final v0 = ixsData[0].ixs[index];
    final vf = ixsData[ixsData.length - 1].ixs[index];
    final nmColor = vf > v0 ? "#00AAFF" : vf < v0 ? "#FF8100" : "#000000";
    return Q("span")
      .add(Q("span")
        .style("color:" + txColor)
        .html(text + ":&nbsp;"))
      .add(Q("span")
        .style("color:" + nmColor)
        .text(Dec.toIso(vf, 2) + "[" + Dec.toIso((vf-v0)*100/v0, 2) + "%]"))
    ;
  }

  function ixsChart (): Domo {
    final labels: Array<String> = [];
    final sets: Array<Array<Option<Float>>> = [];
    for (i in 0...ixsData[0].ixs.length) {
      sets.push(new Array<Option<Float>>());
    }
    final assets0 = mSel == -1
      ? It.from(ixsData[0].invs).reduce(0.0, (r, e) -> r + e) +
        It.from(capitals).reduce(0.0, (r, e) -> r + e)
      : ixsData[0].invs[mSel] + capitals[mSel]
    ;
    final ixs0 = ixsData[0].ixs;
    for (e in ixsData) {
      labels.push("" + e.hour);
      final assets = mSel == -1
        ? It.from(e.invs).reduce(0.0, (r, e) -> r + e) +
          It.from(capitals).reduce(0.0, (r, e) -> r + e)
        : e.invs[mSel] + capitals[mSel]
      ;
      final assetsPc = assets0 > 0 ? (assets - assets0) / assets0 : 0;
      for (i in 0...ixsData[0].ixs.length) {
        final pc = ixs0[i] > 0 ? (e.ixs[i] - ixs0[i]) / ixs0[i] : 0;
        sets[i].push(Some((assetsPc - pc) * 100));
      }
    }
    for (i in 0...3) {
      for (j in 0...ixsData[0].ixs.length) {
        sets[j][i] = Some(0.0);
      }
    }

    final ch = LineChart.mk();
    ch.exArea = new LineChartArea(610, 160, LineChartAreaAtts.mk());
    ch.inPadding.left += 30;
    ch.inPadding.right += 6;
    ch.data = new LineChartData(labels, sets, ch.data.setAtts);
    ch.data.maxMinRound = -2;
    ch.data.setLines.push(new LineChartSetLine(0, LineChartLine.mk()));
    ch.data.setAtts[1].color = "#008000";
    ch.data.drawGrid = (l, i) -> i  == 0
      ? false
      : labels[i-1] == l
        ? false
        : true
    ;
    ch.data.drawLabel = ch.data.drawGrid;

    return ch.mkWg();
  }

  // Control -------------------------------------------------------------------

  function setMenu (manager: Int) {
    Store.put(key, Std.string(manager));
    mSel = manager;
    view();
  }
}
