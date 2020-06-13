// Copyright 31-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Model ranking.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import {Menu} from "../../../dmjs/Menu.js";
import {Table, Col} from "../../../wgs/Table.js"; //eslint-disable-line
import {_} from "../../../I18n.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import Eflea from "../../../data/flea/Eflea.js"; //eslint-disable-line
import Frank from "../../../data/flea/Frank.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    @private
    @param {string} d Date in format YYYYMMDD
    @return string Date in format DD/MM
**/
function dateFormat (d) {
  return d.substring(6) + "/" + d.substring(4, 6);
}

/**
    @private
    @param {!Array<!Eflea>} prev
    @param {!Eflea} e
    @param {number} i
    @return string The name of icon.
**/
function icon (prev, e, i) {
  const name = e.flea.name;
  let pi = -1;
  for (let j = 0; j < prev.length; j++) {
    if (prev[j].flea.name === name) {
      pi = j;
      break;
    }
  }

  if (pi === -1) return "rk-new";
  const df = pi - i;
  if (df > 2) return "rk-up2";
  if (df > 0) return "rk-up";
  if (df === 0) return "rk-eq";
  if (df < -2) return "rk-down2";
  return "rk-down";
}

/**
    Model ranking.
**/
export default class Ranking {
  /**
      @param {!Domo} wg
      @param {!Array<!Col>} cols
      @param {!Array<!Frank>} ranking
      @param {function(?, number):string} link
  **/
  constructor (wg, cols, ranking, link) {
    this._wg = wg;
    this._cols = cols;
    this._ranking = ranking;
    this._link = link;
    this._selSubmenu = ranking.length > 0
      ? dateFormat(ranking[0].date)
      : ""
    ;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const rank = this._ranking;
    const len = rank.length;

    if (len === 0) {
      this._wg
        .removeAll()
        .add($("table")
          .att("align", "center")
          .add($("tr")
            .add($("td")
              .klass("frame")
              .html(_("Without data")))));
      return;
    }

    const wg = $("div");

    const dt = dateFormat(rank[0].date);
    const lopts = [Menu.toption(dt, dt, () => this.show(dt))];
    let efleas = rank[0].efleas;
    let prev = len > 1 ? rank[1].efleas : [];
    for (let i = 1; i < len; ++i) {
      lopts.unshift(Menu.separator());
      const dt = dateFormat(rank[i].date);
      lopts.unshift(Menu.toption(dt, dt, () => this.show(dt)));

      if (dt === this._selSubmenu) {
        efleas = rank[i].efleas;
        prev = len > i + 1 ? rank[i + 1].efleas : [];
      }
    }
    const ropts = [];
    const submenu = new Menu(lopts, ropts, this._selSubmenu);

    const table = efleas.map((e, i) => {
      const r = [
        e, 0, icon(prev, e, i), e.flea.name, e.assets, e.profitsAvg,
        e.profitsVa, e.ev * 1000, e.buys, e.sells
      ];
      e.flea.params.forEach(g => { r.push(g) });
      return r;
    });

    //eslint-disable-next-line
    new Table(wg, this._cols, table, -1, this._link)

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



