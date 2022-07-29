// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Opt;
import data.DailyChart;
import I18n._;

enum CosOrder {NICK; DAY; SIGNAL;}

/// Companies daily charts
class Cos {
  var wg: Domo;
  var type: String;
  var data: Array<DailyChart>;
  var parent: Daily;

  // Can be "all", "sel", "portfolio" or "invN"
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
            final isReverse = parent.reverse;
            final quote = d.quotes[d.quotes.length - 1];
            var pond = 0.0;
            for (i in 0...d.investorsData.length) {
              final id = d.investorsData[i];
              final isSell = id.ref < d.close;
              final v = id.ref < d.close
                ? 1 - (quote - id.ref) / quote
                : 1 - (id.ref - quote) / id.ref
              ;
              pond = switch (type) {
                case "portfolio": {
                  final v2 = id.cought ? 1 - (id.ref - quote) / id.ref : v;
                  id.stocks > 0
                    ? isReverse
                      ? v < pond ? v : pond
                      : v >= pond ? v : pond
                    : pond
                  ;
                }
                case "sel": {
                  final inv = Selection.investor(d.nick);
                  inv == -1
                    ? isReverse
                      ? v < pond ? v : pond
                      : v >= pond ? v : pond
                    : inv == i ? v : pond
                  ;
                }
                default:
                  isReverse
                    ? v < pond ? v : pond
                    : v >= pond ? v : pond;
              }
            }
            return {data: d, pond: pond}
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
                .add(ChSmall.mk(type == "sel", d, (isAdd, nick, inv) -> {
                  changeSel(isAdd, nick, inv);
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

  function changeSel (isAdd: Bool, nick: String, inv: Int): Void {
    if (isAdd) {
      Selection.add(nick, inv);
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
  ///   type: Can be "all", "sel", "portfolio" or "invN"
  ///   chartsData: Charts data.
  public static function mk (
    wg:Domo, parent: Daily, type: String, chartsData: Array<DailyChart>
  ): Void {
    final data = type == "all"
      ? chartsData
      : type == "sel"
        ? chartsData.filter(e -> Selection.contains(e.nick))
        : chartsData.filter(
          e -> It.from(e.investorsData).reduce(0, (r, d) -> r + d.stocks) > 0
        )
    ;

    new Cos(wg, parent, type, data);
  }
}
