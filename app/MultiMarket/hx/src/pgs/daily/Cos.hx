// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import data.DailyChart;
import I18n._;

enum CosOrder {NICK; DAY; SIGNAL;}

/// Companies daily charts
class Cos {
  var wg: Domo;
  var type: String;
  var data: Array<DailyChart>;
  var parent: Daily;

  // type can be "all", "sel" or "portfolio"
  function new (
    wg: Domo, parent: Daily, type: String, data: Array<DailyChart>
  ) {
    this.wg = wg;
    this.type = type;
    this.data = data;
    this.parent = parent;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    switch (parent.order) {
      case NICK:
          data.sort((e1, e2) -> e1.nick > e2.nick ? 1 : -1);
      case DAY:
        final dataPond = data.map(d -> {
            final quote = d.quotes[d.quotes.length - 1];
            final close = d.close;
            return {data: d, pond: (quote - close) / close}
          });
        dataPond.sort((e1, e2) -> e2.pond > e1.pond ? 1 : -1);
        data = dataPond.map(e -> e.data);
      case SIGNAL:
        final dataPond = data.map(d -> {
            final quote = d.quotes[d.quotes.length - 1];
            var max = -1.0;
            for (e in d.managersData) {
              final isSell = e.ref < d.close;
              final dif = type == "portfolio"
                ? e.stocks > 0
                  ? 1 - (quote - e.ref) / quote
                  : -0.01
                : isSell
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

    if (parent.reverse) {
      data.reverse();
    }

    final menu = Q("table")
      .att("align", "center")
      .style("padding-bottom:6px")
      .add(Q("tr")
        .add(Q("td")
          .add(Q("span")
            .html(_("Order by") + ":&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(NICK))
            .klass(parent.order == NICK ? "link frame" : "link")
            .text(_("Nick")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(DAY))
            .klass(parent.order == DAY ? "link frame" : "link")
            .text(_("Day")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeOrder(SIGNAL))
            .klass(parent.order == SIGNAL ? "link frame" : "link")
            .text(_("Signal")))
          .add(Q("span")
            .html("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(e -> changeReverse())
            .klass(parent.reverse ? "link frame" : "link")
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
    Cos.mk(wg, parent, type, data);
  }

  function changeOrder (newOrder: CosOrder): Void {
    parent.setOrder(newOrder);
    view();
  }

  function changeReverse (): Void {
    parent.setReverse(!parent.reverse);
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg: Container.
  ///   parent: Container class.
  ///   type: Can be "all", "sel" or "portfolio"
  ///   chartsData: Charts data.
  public static function mk (
    wg:Domo, parent: Daily, type: String, chartsData: Array<DailyChart>
  ): Void {
    final data = type == "all"
      ? chartsData
      : type == "sel"
        ? chartsData.filter(e -> Selection.contains(e.nick))
        : chartsData.filter(
          e -> It.from(e.managersData).reduce(0, (r, d) -> r + d.stocks) > 0
        )
    ;

    new Cos(wg, parent, type, data);
  }
}
