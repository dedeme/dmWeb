// Copyright 08-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Summary daily charts
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
import Store from "../../dmjs/Store.js";
import {Menu} from "../../dmjs/Menu.js";
import Clock from "../../dmjs/Clock.js";
import {DailyChart} from "../../data/DailyChart.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import ChBig from "./ChBig.js";


const $ = e => Ui.$(e);
const key = "MMarket_chart_manager";

/**
    Companies daily charts
**/
export default class Summary {
  /**
      @param {!Domo} wg
      @param {!Array<!DailyChart>} data
  **/
  constructor (wg, data) {
    this._wg = wg;
    this._data = data;
    const sel = Store.take(key);
    this._mSel = sel === null ? -1 : Number(sel);

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const data = this._data;
    const mSel = this._mSel === -1 ? "All" : _("Inv-") + String(this._mSel);

    const lopts = [
      Menu.toption("All", _("All"), () => this.setMenu(-1))
    ];
    for (let i = 0; i < data[0].accData.length; ++i) {
      const op = _("Inv-") + String(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () => this.setMenu(i)));
    }
    const ropts = [];
    const menu = new Menu(lopts, ropts, mSel);

    const iCo = this._mSel;
    const hours = data[0].hours;
    let investing = 0;
    let yesterdayProfits = 0;
    let todayProfits = 0;
    const profits = hours.map(() => 0);
    for (const co of data) {
      let inv = 0;
      let yprof = 0;
      let prof = 0;
      if (iCo === -1) {
        for (const acc of co.accData) {
          if (acc.stocks > 0) {
            const iv = acc.stocks * acc.price;
            inv += iv;
            yprof += acc.stocks * co.close - iv;
            prof += acc.stocks * co.quotes[0] - iv;
            for (let i = 0; i < profits.length; ++i) {
              profits[i] += acc.stocks * co.quotes[i] - iv;
            }
          }
        }
      } else {
        const acc = co.accData[iCo];
        if (acc.stocks > 0) {
          const iv = acc.stocks * acc.price;
          inv = iv;
          yprof = acc.stocks * co.close - iv;
          prof = acc.stocks * co.quotes[0] - iv;
          for (let i = 0; i < profits.length; ++i) {
            profits[i] += acc.stocks * co.quotes[i] - iv;
          }
        }
      }

      investing += inv;
      yesterdayProfits += yprof;
      todayProfits += prof;
    }

    const dailyProfits = todayProfits - yesterdayProfits;
    const ratio = investing > 0 ? dailyProfits / investing : 0;

    this._wg
      .removeAll()
      .add(menu.wg)
      .add($("div").style("text-align:center;")
        .add($("div")
          .klass("head")
          .style("padding-bottom:8px")
          .html(_("Summary")))
        .add($("div")
          .add($("span")
            .klass("frame")
            .style(
              `font-size:x-large;color:
              ${ratio > 0 ? "#00AAFF" : ratio < 0 ? "#FF8100" : "#000000"}`
            )
            .html(
              ` ${new Dec(ratio * 100, 2).toIso()}% |` +
              ` ${new Dec(dailyProfits, 2).toIso()}€ `
            )))
        .add(ChBig.mk(hours, profits, ratio))
        .add(new Clock().mkWg().klass("frame")
          .style(
            "background:radial-gradient(#000333,#e6f6f6);" +
            "margin-top: 8px;"
          )))

    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {number} manager
      @return void
  **/
  setMenu (manager) {
    Store.put(key, String(manager));
    this._mSel = manager;
    this.view();
  }

}
