// Copyright 11-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting profits.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {Menu} from "../../dmjs/Menu.js";
import {_} from "../../I18n.js";
import Cts from "../../data/Cts.js";
import ProfitsWg from "./wgs/ProfitsWg.js";

const $ = e => Ui.$(e);

/**
    Accounting profits.
**/
export default class Profits {
  /**
      @param {!Domo} wg
      @param {!Array<?>} data Has the following structure:
        [
          [
            date:string [total:number, acc:number, risk:number],
            date [total, acc, risk],
            ... (from after to before)
            until N dates
          ]
          [
            date [total, acc, risk],
            date [total, acc, risk],
            ... (from after to before)
            until N dates
          ]
          ...
          until mangers number.
        ]
  **/
  constructor (wg, data) {
    this._wg = wg;
    this._data = data;
    this._mSel = -1;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const wg = $("div");
    const data = this._data;
    const mSel = this._mSel === -1 ? "All" : _("Inv-") + String(this._mSel);

    const lopts = [
      Menu.toption("All", _("All"), () => this.setMenu(-1))
    ];
    for (let i = 0; i < data.length; ++i) {
      const op = _("Inv-") + String(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () => this.setMenu(i)));
    }
    const ropts = [];
    const menu = new Menu(lopts, ropts, mSel);

    let d = [];
    if (this._mSel === -1) {
      for (let i = 0; i < data[0].length; ++i) {
        let total = 0;
        let acc = 0;
        let risk = 0;
        for (let inv = 0; inv < data.length; ++inv) {
          if (data[inv][i]) {
            const values = data[inv][i][1];
            total += values[0];
            acc += values[1];
            risk += values[2];
          }
        }
        d.push([data[0][i][0], [total, acc, risk]]);
      }
    } else {
      d = data[this._mSel];
    }
    ProfitsWg.mk(wg, d);

    this._wg
      .removeAll()
      .add(menu.wg)
      .add(wg)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {number} manager
      @return void
  **/
  setMenu (manager) {
    this._mSel = manager;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return !Promise<!Profits>
  **/
  static async mk (wg) {
    const rp = await Cts.client.send({
      "module": "acc",
      "source": "profits",
      "rq": "idata"
    });

    const data = rp["data"];
    return new Profits(wg, data);
  }

}
