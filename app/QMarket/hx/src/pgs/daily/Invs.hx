// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Opt;
import data.DailyChart;
import I18n._;

enum InvsOrder {SELL;BUY;}

/// Investors daily charts
class Invs {
  var wg: Domo;
  var inv: Int;
  var dailyData: Array<DailyChart>;
  var parent: Daily;

  function new (
    wg: Domo, parent: Daily, inv: Int, data: Array<DailyChart>
  ) {
    this.wg = wg;
    this.inv = inv;
    dailyData = data;
    this.parent = parent;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    function psell (quote: Float, ref: Float): Float {
      return 1 - (quote - ref) / quote;
    }

    function pbuy (quote: Float, ref: Float): Float {
      return 1 - (ref - quote) / ref;
    }

    final data = parent.iorder == SELL
      ? It.from(dailyData)
          .filter(e -> e.investorsData[inv].stocks > 0)
          .sort((e1, e2) ->
            psell(e1.quotes[e1.quotes.length - 1], e1.investorsData[inv].ref) <
            psell(e2.quotes[e1.quotes.length - 1], e2.investorsData[inv].ref)
            ? 1 : -1)
          .to()
      : It.from(dailyData)
          .filter(e ->
            e.investorsData[inv].stocks == 0 &&
            e.close < e.investorsData[inv].ref)
          .sort((e1, e2) ->
            pbuy(e1.quotes[e1.quotes.length - 1], e1.investorsData[inv].ref) <
            pbuy(e2.quotes[e1.quotes.length - 1], e2.investorsData[inv].ref)
            ? 1 : -1)
          .to()
    ;

    final menu = Q("table")
      .att("align", "center")
      .style("padding-bottom:6px")
      .add(Q("tr")
        .add(Q("td")
          .add(Ui.link(e -> changeOrder(SELL))
            .klass(parent.iorder == SELL ? "link frame" : "link")
            .text(_("Sell")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(BUY))
            .klass(parent.iorder == BUY ? "link frame" : "link")
            .text(_("Buy")))))
    ;


    final table = (Q("table")
      .att("align", "center")
      .klass("frame")
      .adds(data.length == 0
        ? [Q("tr")
          .add(Q("td")
            .text(_("No selected company")))
        ]
        : It.range(Std.int((data.length - 1) / 3) + 1).map(row -> {
          return Q("tr")
            .adds(It.range(3).map(col -> {
              final ix = row * 3 + col;
              if (ix >= data.length) {
                return Q("td");
              }
              final d = data[ix];
              return Q("td")
                .add(ChSmall.mk(d, (isAdd, nick) -> {
                  changeSel(isAdd, nick);
                }))
              ;
            }).to())
          ;
        }).to()))
    ;

    wg
      .removeAll()
      .add(menu)
      .add(table)
    ;
  }

  // Control -------------------------------------------------------------------

  function changeSel (isAdd: Bool, nick: String): Void {
    if (isAdd) {
      Selection.add(nick);
    } else {
      Selection.remove(nick);
    }
    Invs.mk(wg, parent, inv, dailyData);
  }

  function changeOrder (newOrder: InvsOrder): Void {
    parent.setIorder(newOrder);
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  ///   parent: Daily page.
  ///   inv: Investor identifier (0...)
  ///   chartsData: Charts data.
  public static function mk (
    wg:Domo, parent: Daily, inv: Int, chartsData: Array<DailyChart>
  ): Void {
    final data = chartsData;

    new Invs(wg, parent, inv, data);
  }
}
