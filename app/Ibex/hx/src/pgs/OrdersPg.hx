// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.It;
import dm.Dec;
import dm.Str;
import dm.Vmenu;
import dm.Tp;
import data.Market;
import data.Order;
import I18n._;

/// Orders page.
class OrdersPg {
  final wg: Domo;
  final market: Market;
  final models: Array<String>;
  var selModel: String;

  public function new (wg: Domo, market: Market) {
    this.wg = wg;
    this.market = market;

    models = It.from(market.models).filter(m -> m.id != "CM")
      .map(m -> m.id)
      .sort(Str.compare)
      .to();

    selModel = models[0] + " (Pond)";
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final ops = [Vmenu.title(_("Models"))];
    for (m in models) {
      final e = m + " (Pond)";
      ops.push(Vmenu.option(e, e, () -> select(e)));
      final e2 = m + " (Prop)";
      ops.push(Vmenu.option(e2, e2, () -> select(e2)));
    }
    final vmenu = new Vmenu(ops, selModel);

    final mdId = Str.left(selModel, selModel.indexOf(" "));
    final isPond = Str.sub(selModel, -5, -1) == "Pond";
    final rs = Opt.eget(It.from(market.results).find(r -> r.model == mdId));
    final orders: Array<Tp<String, Order>> = [];
    for (nkRs in rs.results) {
      if (nkRs.nick == "IBEX") continue;

      final simRs = isPond ? nkRs.pondResults : nkRs.propResults;
      for (o in simRs.orders) orders.push(new Tp(nkRs.nick, o));
    }
    orders.sort((o1, o2) -> o1.e2.date > o2.e2.date ? 1 : -1);


    final trs = [Q("tr")
      .add(Q("th").text(_("Date")))
      .add(Q("th").style("width:60px").text(_("Buy")))
      .add(Q("th").style("width:60px").text(_("Sale")))
      .add(Q("th").style("width:400px").text(_("Portfolio")))
      .add(Q("th").text(_("Profits")))
    ];

    final profits = 0.0;
    final pf: Array<String> = [];
    for (nko in orders) {
      final nk = nko.e1;
      final o = nko.e2;

      if (o.bs == "b") {
        pf.push(nk);
      } else {
        final ix = pf.indexOf(nk);
        if (ix != -1) pf.splice(ix, 1);
        profits += o.profits;
      }
      pf.sort(Str.compare);

      trs.push(Q("tr")
        .add(Q("td").klass("border").text(o.date))
        .add(Q("td").klass("border").text(o.bs == "b" ? nk : ""))
        .add(Q("td").klass("border").text(o.bs == "s" ? nk : ""))
        .add(Q("td").klass("border").text(pf.join(", ")))
        .add(Q("td")
          .klass("border")
          .style("text-align:right")
          .text(Dec.toIso(profits, 2)))
      );
    }

    wg
      .removeAll()
      .add(Q("table")
      .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top;")
            .add(vmenu.wg))
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .style("border-collapse : collapse;")
              .adds(trs)))))
    ;

  }

  // Control -------------------------------------------------------------------

  function select (md: String): Void {
    selModel = md;
    show();
  }
}
