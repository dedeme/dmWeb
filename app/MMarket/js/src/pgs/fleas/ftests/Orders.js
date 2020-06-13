// Copyright 25-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Dec from "../../../dmjs/Dec.js";
import {_, _args} from "../../../I18n.js";
import Params from "../../../wgs/Params.js";
import OrdersWg from "./wgs/Orders.js";
import Msg from "../../../wgs/Msg.js";
import Cts from "../../../data/Cts.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Forder from "../../../data/flea/Forder.js";
import Rs from "../../../data/flea/AssetsRs.js";

const $ = e => Ui.$(e);

/**
    Orders fleas tests page.
**/
export default class Orders {
  /**
      @param {!Domo} wg
      @param {!Fmodel} model
  **/
  constructor (wg, model) {
    this._wg = wg;
    this._model = model;

    this._wgParams = new Params(
      model.parNames, model.parMins, model.parMaxs, "par", "showBt"
    );
    this._ordersDiv = $("div");
    this._resultDiv = $("div");
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .style("width:50%;text-align:center")
            .add($("div")
              .klass("head")
              .text(_("Parameters")))
            .add($("table")
              .att("align", "center")
              .add($("tr").add($("td")
                .add(this._wgParams.wg))))))
        .add($("tr")
          .add($("td")
            .style("text-align:center")
            .add($("div")
              .style("height:5px"))
            .add($("button")
              .att("id", "showBt")
              .text(_("Show"))
              .on("click", () => this.showOrders()))
            .add($("div")
              .style("height:5px"))
            .add(this._ordersDiv.removeAll())
            .add($("div")
              .style("height:5px"))
            .add(this._resultDiv.removeAll())))
      );
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async showOrders () {
    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "ftests/orders",
      "rq": "ordersData",
      "modelId": this._model.id,
      "params": this._wgParams.value
    });

    if (!rp["ok"]) {
      Msg.error(Cts.failMsg, () => {});
      return;
    }

    const orders = rp["orders"].map(e => Forder.fromJs(e));

    this._ordersDiv.removeAll()
      .add(new OrdersWg(orders, rp["nicks"], rp["lastCloses"]).wg
        .klass("frame0"));

    const rs = Rs.fromJs(rp["assets"]);
    this._resultDiv.removeAll()
      .add($("div")
        .html(_args(
          _("Calculated by server:<br>Assets: %0. Buys: %1. Sells: %2"),
          new Dec(rs.assets, 2).toIso(),
          new Dec(rs.buys, 0).toIso(),
          new Dec(rs.sells, 0).toIso()
        )));
  }

}
