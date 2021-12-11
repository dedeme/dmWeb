// Copyright 15-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import data.Cts;
import data.model.Model;
import data.model.Eval;
import data.model.Order;
import wgs.Params;
import wgs.Msg;
import I18n._;
import I18n._args;
import pgs.models.tests.wgs.OrdersWg;

/// Model Orders tests page.
class Orders {
  var wg: Domo;
  var qlevel: Int;
  var wgParams: Params;

  final ordersDiv = Q("div");
  final resultDiv = Q("div");

  /// Constructor
  ///   wg   : Container.
  ///   model: Flea model.
  public function new (wg: Domo, qlevel: Int) {
    this.wg = wg;
    this.qlevel = qlevel;

    wgParams = new Params(_("Jump"), "par", "showBt");

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(Q("table").klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:50%;text-align:center")
            .add(Q("div")
              .klass("head")
              .text(_("Parameters")))
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr").add(Q("td")
                .add(wgParams.wg))))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Q("div")
              .style("height:5px"))
            .add(Q("button")
              .att("id", "showBt")
              .text(_("Show"))
              .on(CLICK, e -> showOrders()))
            .add(Q("div")
              .style("height:5px"))
            .add(ordersDiv.removeAll())
            .add(Q("div")
              .style("height:5px"))
            .add(resultDiv.removeAll())))
      );
  }

  // Control -------------------------------------------------------------------

  function showOrders () {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("tests/orders"),
      "rq" => Js.ws("ordersData"),
      "qlevel" => Js.wi(qlevel),
      "paramId" => Js.wi(Math.round(wgParams.value * Cts.rangesToParam))
    ], rp -> {
      final orders = rp["orders"].ra().map(e -> Order.fromJs(e));
      final nicks = rp["nicks"].ra().map(e -> e.rs());
      final lastCloses = rp["lastCloses"].ra().map(e -> e.rf());

      ordersDiv
        .removeAll()
        .add(new OrdersWg(orders, nicks, lastCloses).wg
          .klass("frame0"));

      final ev = Eval.fromJs(rp["eval"]);
      resultDiv
        .removeAll()
        .add(Q("div")
          .html(_args(
            _("Calculated by server:<br>Assets: %0. Buys: %1. Sells: %2"),
            [ Dec.toIso(ev.assets, 2),
              Dec.toIso(ev.buys, 0),
              Dec.toIso(ev.sales, 0)
            ]
          )));
    });
  }

}
