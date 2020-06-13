// Copyright 02-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Investors ranking.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import B64 from "../../dmjs/B64.js";
import {Menu} from "../../dmjs/Menu.js";
import {Table, Col} from "../../wgs/Table.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Cts from "../../data/Cts.js"; //eslint-disable-line
import Investor from "../../data/flea/Investor.js"; //eslint-disable-line
import Irank from "../../data/flea/Irank.js"; //eslint-disable-line

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
    @param {!Array<!Investor>} prev
    @param {!Investor} inv
    @param {number} i
    @return string The name of icon.
**/
function icon (prev, inv, i) {
  const name = inv.eflea.flea.name;
  let pi = -1;
  for (let j = 0; j < prev.length; j++) {
    if (prev[j].eflea.flea.name === name) {
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
    Investors ranking.
**/
export default class Ranking {
  /**
      @param {!Domo} wg
      @param {!Array<!Col>} cols
      @param {!Array<!Irank>} ranking
  **/
  constructor (wg, cols, ranking) {
    this._wg = wg;
    this._cols = cols;
    this._ranking = ranking;
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
    let invs = rank[0].invs;
    let prev = len > 1 ? rank[1].invs : [];
    for (let i = 1; i < len; ++i) {
      lopts.unshift(Menu.separator());
      const dt = dateFormat(rank[i].date);
      lopts.unshift(Menu.toption(dt, dt, () => this.show(dt)));

      if (dt === this._selSubmenu) {
        invs = rank[i].invs;
        prev = len > i + 1 ? rank[i + 1].invs : [];
      }
    }
    const ropts = [];
    const submenu = new Menu(lopts, ropts, this._selSubmenu);

    const table = invs.map((inv, i) => {
      const parDecs = inv.model.parDecs;
      const e = inv.eflea;
      const parValues = e.flea.params;
      const r = [
        inv, 0, icon(prev, inv, i), inv.name, e.assets, e.profitsAvg,
        e.profitsVa, e.ev * 1000, e.buys, e.sells
      ];
      const len = r.length - 1;
      for (let i = 0; i < this._cols.length - len; ++i) {
        r.push(i < parValues.length
          ? Cts.nformat(parValues[i], parDecs[i])
          : ""
        );
      }
      return r;
    });

    //eslint-disable-next-line
    new Table(wg, this._cols, table, -1, (e, i) => this.link(e, i))

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

  /**
      @private
      @param {!Investor} inv Investor
      @param {number} i Column index.
      @return string
  **/
  link (inv, i) {
    return `?fleas&${inv.model.id}&charts&${i === 1}&` +
           `${B64.encode(JSON.stringify(inv.eflea.toJs()))}`
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return !Promise<!Ranking>
  **/
  static async mk (wg) {
    const rp = await Cts.client.send({
      "module": "ranking",
      "source": "ranking",
    });
    const /**!Array<!Irank>**/ ranking =
      rp["ranking"].map(e => Irank.fromJs(e));

    const nPar = ranking.reduce(
      (r, irk) => {
        const n = irk.invs.reduce(
          (r, inv) => {
            const n = inv.eflea.flea.params.length;
            return n > r ? n : r;
          },
          0
        );
        return n > r ? n : r;
      },
      0
    );

    const cols = [
      new Col(_("Nº"), Col.COUNTER, 0, false, false),
      new Col("", Col.ICON, -1, true, false),
      new Col(_("Id"), Col.STRING, -1, true, false),
      new Col(_("Assets"), Col.NUMBER, 2, false, false),
      new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
      new Col(
        _("Pf. Var"), Col.NUMBER, 4,
        false, false
      ),
      new Col(
        _("Eval."), Col.NUMBER, 2,
        false, false
      ),
      new Col(
        _("Buys"), Col.NUMBER, 0,
        false, false
      ),
      new Col(
        _("Sells"), Col.NUMBER, 0,
        false, false
      )
    ];
    for (let i = 0; i < nPar; ++i) {
      cols.push(new Col(`P. ${i + 1}`, Col.P_STRING, -1, false, false));
    }

    return new Ranking(wg, cols, ranking);
  }

}



