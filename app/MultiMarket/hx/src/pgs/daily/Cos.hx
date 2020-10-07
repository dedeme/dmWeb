// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import data.DailyChart;
import I18n._;

/// Companies daily charts
class Cos {
  var wg: Domo;
  var type: String;
  var data: Array<DailyChart>;
  var orderByNick: Bool;
  var reverse: Bool;

  // type can be "all", "sel" or "portfolio"
  function new (wg: Domo, type: String, data: Array<DailyChart>) {
    this.wg = wg;
    this.type = type;
    this.data = data;
    orderByNick = false;
    reverse = false;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    if (orderByNick) {
      data.sort((e1, e2) -> e1.nick > e2.nick ? 1 : -1);
    } else {
      final dataPond = data.map(d -> {
        final quote = d.quotes[d.quotes.length - 1];
        var max = -1.0;
        for (e in d.managersData) {
          final isSell = e.ref < d.close;
          final dif = isSell
            ? 1 - (quote - e.ref) / quote
            : 1 - (e.ref - quote) / e.ref
          ;
          final p = 100 * dif;
          if (p > max) max = p;
        };
        return {data: d, pond: max}
      });
      dataPond.sort((e1, e2) -> e2.pond > e1.pond ? 1 : -1);
      data = dataPond.map(e -> e.data);
    }
    if (reverse) {
      data.reverse();
    }

    final menu = Q("table")
      .att("align", "center")
      .style("padding-bottom:6px")
      .add(Q("tr")
        .add(Q("td")
          .add(Q("span")
            .html(_("Order by") + ":&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(true))
            .klass(orderByNick ? "link frame" : "link")
            .text(_("Nick")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(false))
            .klass(!orderByNick ? "link frame" : "link")
            .text(_("Signal")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeReverse())
            .klass(reverse ? "link frame" : "link")
            .text(_("Reverse")))))
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
    Cos.mk(wg, type, data);
  }

  function changeOrder (byNick: Bool): Void {
    orderByNick = byNick;
    view();
  }

  function changeReverse (): Void {
    reverse = !reverse;
    view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {string} type All, selected or portfolio companies.
      @param {!Array<!DailyChart>} chartsData
      @return !Cos
  **/
  /// Constructor
  ///   wg: Container.
  ///   type: Can be "all", "sel" or "portfolio"
  ///   chartsData: Charts data.
  public static function mk (
    wg:Domo, type: String, chartsData: Array<DailyChart>
  ): Void {
    final data = type == "all"
      ? chartsData
      : type == "sel"
        ? chartsData.filter(e -> Selection.contains(e.nick))
        : chartsData.filter(
          e -> It.from(e.managersData).reduce(0, (r, d) -> r + d.stocks) > 0
        )
    ;

    new Cos(wg, type, data);
  }
}
