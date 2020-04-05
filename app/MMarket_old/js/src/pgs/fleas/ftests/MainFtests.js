// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Main fleas tests page.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import {Menu} from "../../../dmjs/Menu.js";
import {_} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import References from "./References.js";
import Orders from "./Orders.js";
import Selection from "./Selection.js";

const $ = e => Ui.$(e);

/**
    Main Fleas Tests page.
**/
export default class MainFtests {
  /**
      @param {!Client} client
      @param {!Fmodel} model
  **/
  constructor (client, model) {
    this._client = client;
    this._model = model;
    this._selSubmenu = "references";

    this._wgChild = $("div");
    this._wg = $("div");
    this.showReferences();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const lopts = [
      Menu.toption("references", _("References"), () => this.showReferences()),
      Menu.separator(),
      Menu.toption("orders", _("Orders"), () => this.showOrders()),
      Menu.separator(),
      Menu.toption("selection", _("Selection"), () => this.showSelection())
    ];
    const subMenu = new Menu(lopts, [], this._selSubmenu);

    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add(subMenu.wg)
            .add(this._wgChild))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async showReferences () {
    this._selSubmenu = "references";
    const pg = await References.mk(this._client, this._model);
    this._wgChild.removeAll().add(pg.wg);
    this.view();
  }

  /**
      @private
  **/
  showOrders () {
    this._selSubmenu = "orders";
    const pg = new Orders(this._client, this._model);
    this._wgChild.removeAll().add(pg.wg);
    this.view();
  }

  /**
      @private
  **/
  async showSelection () {
    this._selSubmenu = "selection";
    const pg = await Selection.mk(this._client, this._model);
    this._wgChild.removeAll().add(pg.wg);
    this.view();
  }

}


