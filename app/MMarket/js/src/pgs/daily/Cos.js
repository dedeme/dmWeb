// Copyright 08-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Companies daily charts
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import It from "../../dmjs/It.js";
import {DailyChart} from "../../data/DailyChart.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Selection from "./Selection.js";
import ChSmall from "./ChSmall.js";


const $ = e => Ui.$(e);

/**
    Companies daily charts
**/
export default class Cos {
  /**
      @param {!Domo} wg
      @param {string} type All, selected or portfolio companies.
      @param {!Array<!DailyChart>} data
  **/
  constructor (wg, type, data) {
    this._wg = wg;
    this._type = type;
    this._data = data;
    this._orderByNick = false;
    this._reverse = false;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    let data = this._data;
    if (this._orderByNick) {
      data.sort((e1, e2) => e1.nick > e2.nick ? 1 : -1);
    } else {
      const dataPond = data.map(d => {
        const quote = d.quotes[0];
        let max = -1;
        d.accData.forEach(e => {
          const isSell = e.ref < d.close;
          const dif = isSell
            ? 1 - (quote - e.ref) / quote
            : 1 - (e.ref - quote) / e.ref
          ;
          const p = 100 * dif;
          if (p > max) max = p;
        });
        return [d, max];
      });
      dataPond.sort((e1, e2) => e2[1] - e1[1]);
      data = dataPond.map(e => e[0]);
    }
    if (this._reverse) {
      data.reverse();
    }

    const menu = $("table")
      .att("align", "center")
      .style("padding-bottom:6px")
      .add($("tr")
        .add($("td")
          .add($("span")
            .html(_("Order by") + ":&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(() => { this.changeOrder(true) })
            .klass(this._orderByNick ? "link frame" : "link")
            .text(_("Nick")))
          .add($("span")
            .html("&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(() => { this.changeOrder(false) })
            .klass(!this._orderByNick ? "link frame" : "link")
            .text(_("Signal")))
          .add($("span")
            .html("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"))
          .add(Ui.link(() => { this.changeReverse() })
            .klass(this._reverse ? "link frame" : "link")
            .text(_("Reverse")))))
    ;


    const table = ($("table")
      .att("align", "center")
      .klass("frame")
      .adds(data.length === 0
        ? [$("tr")
          .add($("td")
            .text(_("No selected company")))
        ]
        : [...It.range((((data.length - 1) / 3)|0) + 1).map(row => {
          return $("tr")
            .adds([...It.range(3).map(col => {
              const ix = row * 3 + col;
              if (ix >= data.length) {
                return $("td");
              }
              const d = data[ix];
              return $("td")
                .add(ChSmall.mk(d, (isAdd, nick) => {
                  this.changeSel(isAdd, nick);
                }))
              ;
            })])
          ;
        })]))
    ;

    this._wg
      .removeAll()
      .add(menu)
      .add(table)
    ;
  }

  // Action --------------------------------------------------------------------

  /**
      @private
      @param {boolean} isAdd
      @param {string} nick
      @return void
  **/
  changeSel (isAdd, nick) {
    if (isAdd) {
      Selection.add(nick);
    } else {
      Selection.remove(nick);
    }
    Cos.mk(this._wg, this._type, this._data);
  }

  /**
      @private
      @param {boolean} byNick
      @return void
  **/
  changeOrder (byNick) {
    this._orderByNick = byNick;
    this.view();
  }

  /**
      @private
  **/
  changeReverse () {
    this._reverse = !this._reverse;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {string} type All, selected or portfolio companies.
      @param {!Array<!DailyChart>} chartsData
      @return !Cos
  **/
  static mk (wg, type, chartsData) {
    const data = type === "all"
      ? chartsData
      : type === "sel"
        ? chartsData.filter(e => Selection.contains(e.nick))
        : chartsData.filter(
          e => e.accData.reduce((r, d) => r + d.stocks, 0) > 0
        )
    ;

    return new Cos(wg, type, data);
  }
}
