// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import data.Model;
import data.Order;
import data.AssetsRs;
import wgs.Params;
import wgs.Msg;
import I18n._;
import I18n._args;
import pgs.models.tests.wgs.OrdersWg;

/// Orders fleas tests page.
class Orders {
  var wg: Domo;
  var model: Model;
  var wgParams: Params;

  final ordersDiv = Q("div");
  final resultDiv = Q("div");

  /// Constructor
  ///   wg   : Container.
  ///   model: Flea model.
  public function new (wg: Domo, model: Model) {
    this.wg = wg;
    this.model = model;

    wgParams = new Params(
      model.paramNames, model.paramMins, model.paramMaxs, "par", "showBt"
    );

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
      "modelId" => Js.ws(model.id),
      "params" => Js.wa(wgParams.value.map(e -> Js.wf(e)))
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg);
        return;
      }

      final orders = rp["orders"].ra().map(e -> Order.fromJs(e));
      final nicks = rp["nicks"].ra().map(e -> e.rs());
      final lastCloses = rp["lastCloses"].ra().map(e -> e.rf());

      ordersDiv
        .removeAll()
        .add(new OrdersWg(orders, nicks, lastCloses).wg
          .klass("frame0"));

      final rs = AssetsRs.fromJs(rp["assets"]);
      resultDiv
        .removeAll()
        .add(Q("div")
          .html(_args(
            _("Calculated by server:<br>Assets: %0. Buys: %1. Sells: %2"),
            [ Dec.toIso(rs.assets, 2),
              Dec.toIso(rs.buys, 0),
              Dec.toIso(rs.sells, 0)
            ]
          )));
    });
  }

}