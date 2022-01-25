// Copyright 14-Nov-2021 ºDeme
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
import data.Cts;
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
            yprof += acc.stocks * co.close - iv;
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
          yprof = acc.stocks * co.close - iv;
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

  // Control -------------------------------------------------------------------

  function setMenu (manager: Int) {
    Store.put(key, Std.string(manager));
    mSel = manager;
    view();
  }
}
