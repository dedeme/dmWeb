// Copyright 07-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting information, main page.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {_} from "../../I18n.js";
import {Menu} from "../../dmjs/Menu.js";
import Dmenu from "../../wgs/Dmenu.js"; //eslint-disable-line
import Companies from "./Companies.js";
import Balance from "./Balance.js";
import Trading from "./Trading.js";
import Profits from "./Profits.js";

const $ = e => Ui.$(e);

export default class Acc {

  /**
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @return {!Promise<!Acc>}
  **/
  constructor (wg, dmenu, lcPath) {
    this._wg = wg;
    this._dmenu = dmenu;
    this._lcPath = lcPath;

    if (lcPath.length === 0) lcPath.push("profits");
    this._mSel = lcPath[0];
    this._lcPath = lcPath;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const wg = $("div");

    const lopts = [
      this._dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("companies", _("Companies"), "acc"),
      Menu.separator(),
      Menu.tlink("balance", _("Balance"), "acc"),
      Menu.separator(),
      Menu.tlink("trading", _("Trading"), "acc"),
      Menu.separator(),
      Menu.tlink("profits", _("Profits"), "acc")
    ];
    const ropts = [];
    this._dmenu.downMenu = new Menu(lopts, ropts, this._mSel);

    switch (this._mSel) {
    case "companies":
      Companies.mk(wg);
      break;
    case "balance":
      Balance.mk(wg);
      break;
    case "trading":
      Trading.mk(wg);
      break;
    case "profits":
      Profits.mk(wg);
      break;
    default:
      Profits.mk(wg);
    }

    this._wg
      .removeAll()
      .add(wg)
    ;
  }

}
