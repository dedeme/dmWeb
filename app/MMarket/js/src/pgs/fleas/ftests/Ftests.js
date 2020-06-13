// Copyright 21-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import {Menu} from "../../../dmjs/Menu.js";
import Dmenu from "../../../wgs/Dmenu.js"; //eslint-disable-line
import {_} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Selection from "./Selection.js";
import Orders from "./Orders.js";
import References from "./References.js";

const $ = e => Ui.$(e);

/**
    Fleas tests main page.
**/
export default class Ftests {
  /**
      @param {!Domo} wg
      @param {!Fmodel} model
  **/
  constructor (wg, model) {
    this._wg = wg;
    this._model = model;
    this._selSubmenu = "references";

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const wg = $("div");

    const lopts = [
      Menu.toption("references", _("References"),
        () => this.show("references")
      ),
      Menu.separator(),
      Menu.toption("orders", _("Orders"), () => this.show("orders")),
      Menu.separator(),
      Menu.toption("selection", _("Selection"), () => this.show("selection"))
    ];
    const ropts = [];
    const submenu = new Menu(lopts, ropts, this._selSubmenu);

    switch (this._selSubmenu) {
    case "references":
      References.mk(wg, this._model);
      break;
    case "orders":
      new Orders(wg, this._model); //eslint-disable-line
      break;
    case "selection":
      Selection.mk(wg, this._model);
      break;
    default:
      alert("references");
      break;
    }

    this._wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .add(submenu.wg)
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {string} option Menu option.
      @return {void}
  **/
  show (option) {
    this._selSubmenu = option;
    this.view();
  }

}



