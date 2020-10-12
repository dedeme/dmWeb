// Copyright 18-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting page.
**/

import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import {Menu} from "../../../dmjs/Menu.js";
import {_} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import All from "./All.js";
import Editor from "./Editor.js";

const $ = e => Ui.$(e);

export default class Acc {
  /**
    @param {!Domo} wg
    @param {number} investors
    @param {!Array<string>} years
    @param {!Array<?>} anns
    @param {number} cash
  **/
  constructor (wg, investors, years, anns, cash) {
    this._wg = wg;
    this._investors = investors;
    this._years = years;
    this._anns = anns;
    this._cash = cash;

    this._body = $("div");
    this.view();
    this._menuSel = "all";
    this.all();
  }

  // View ----------------------------------------------------------------------

  /**
    @private
    @return void
  **/
  view () {
    const lopts = [
      Menu.toption("all", _("All"), () => this.all())
    ];
    for (let i = 0; i < this._investors; ++i) {
      const lb = `${_("Inv")}-${i}`;
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(lb, lb, () => { this.investor(i) }));
    }
    const menu = new Menu(lopts, [], this._menuSel);

    this._wg
      .removeAll()
      .add(menu.wg)
      .add(this._body)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
    @private
  **/
  all () {
    // eslint-disable-next-line
    new All(this._body, this._years, this._anns, this._cash);
    this._menuSel = "all";
    this.view();
  }

  /**
    @private
    @param {number} i Investor identifier.
  **/
  investor (i) {
    Editor.mk(this._body, this._years[0], i);
    this._menuSel = `${_("Inv")}-${i}`;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return {!Promise<!Acc>}
  **/
  static async mk (wg) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "acc",
      "rq": "idata"
    });
    const /** number */ investors = rp["investors"];
    const /** !Array<string> */ years = rp["years"];
    const /** !Array<?> */ anns = rp["anns"];
    const /** number */ cash = rp["cash"];
    return new Acc(wg, investors, years, anns, cash);
  }

}
