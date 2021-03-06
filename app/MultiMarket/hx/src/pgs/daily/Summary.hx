// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Store;
import dm.Menu;
import dm.Clock;
import data.DailyChart;
import I18n._;

/// Daily charts summary
class Summary {
  static var key = "MultiMarket_chart_manager";

  var wg: Domo;
  var data: Array<DailyChart>;
  var mSel: Int;

  /// Constructor.
  ///   wg  : Container.
  ///   data: All the comapanies data.
  public function new (wg: Domo, data: Array<DailyChart>) {
    this.wg = wg;
    this.data = data;
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
    for (i in 0...data[0].managersData.length) {
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
        for (acc in co.managersData) {
          if (acc.stocks > 0) {
            final iv = acc.stocks * acc.price;
            inv += iv;
            yprof += acc.stocks * co.close - iv;
            prof += acc.stocks * co.quotes[co.quotes.length - 1] - iv;
            for (i in 0...profits.length) {
              profits[i] += acc.stocks * co.quotes[i] - iv;
            }
          }
        }
      } else {
        final acc = co.managersData[iCo];
        if (acc.stocks > 0) {
          final iv = acc.stocks * acc.price;
          inv = iv;
          yprof = acc.stocks * co.close - iv;
          prof = acc.stocks * co.quotes[co.quotes.length - 1] - iv;
          for (i in 0...profits.length) {
            profits[i] += acc.stocks * co.quotes[i] - iv;
          }
        }
      }

      investing += inv;
      yesterdayProfits += yprof;
      todayProfits += prof;
    }

    final dailyProfits = todayProfits - yesterdayProfits;
    final ratio = investing > 0 ? dailyProfits / investing : 0;

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
              .add(new Clock().wg
                .klass("frame")
                .style(
                  "background:radial-gradient(#000333,#e6f6f6);" +
                  "margin-top: 8px;"
                ))))))
    ;
  }

  // Control -------------------------------------------------------------------

  function setMenu (manager: Int) {
    Store.put(key, Std.string(manager));
    mSel = manager;
    view();
  }
}
