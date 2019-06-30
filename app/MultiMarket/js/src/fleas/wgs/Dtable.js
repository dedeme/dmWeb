// Copyright 27-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";
import DateDm from "../../dmjs/DateDm.js";
import Dec from "../../dmjs/Dec.js";
import It from "../../dmjs/It.js";
import {_} from "../../I18n.js";

import Main from "../../Main.js";
import Wcharts from "../wgs/Wcharts.js";

const $ = Ui.$;

const DATE_MODEL = 0;
const FLEA = 1;
const PARAMS = 2;
const ASSETS = 3;
const AVG = 4;
const MDV = 5;
const SEL = 6;
const BUYS = 7;
const SELLS = 8;
const PARAM_COLS = 9;

/** Data table. */
export default class Dtable {
  /**
   * @param {boolean} isBests
   * @param {boolean} isChampions If isChampions is true, isBest is true too.
   * @param {!Array<?>} params Flea model parameters data.
   * @param {!Array<?>} entries Table elements.
   */
  constructor (isBests, isChampions, params, entries) {
    /**
     * @private
     * @type {boolean}
     */
    this._isBests = isBests;

    /**
     * @private
     * @type {boolean}
     */
    this._isChampions = isChampions;

    /**
     * @private
     * @type {!Array<?>}
     */
    this._params = params;

    /**
     * @private
     * @type {!Array<?>}
     */
    this._entries = entries.map(e => {
      let date, flea, assets, buys, sells, avg, va, sel, paramsValues;
      /* eslint-disable */
      if (isBests) {
        date =          e[0];
        flea =          e[1][0][0];
        assets =        e[1][0][1][0];
        buys =          e[1][0][1][1];
        sells =         e[1][0][1][2];
        avg =           e[1][0][2][0];
        va =            e[1][0][2][1];
        sel =           e[1][0][2][2];
        paramsValues =  e[1][1];
      } else {
        date =          "";
        flea =          e[0][0];
        assets =        e[0][1][0];
        buys =          e[0][1][1];
        sells =         e[0][1][2];
        avg =           e[0][2][0];
        va =            e[0][2][1];
        sel =           e[0][2][2];
        paramsValues =  e[1];
      }
      /* eslint-enable */
      return [date, flea, paramsValues, assets, avg, va, sel, buys, sells];
    });

    /**
     * @private
     * type {"Domo}
     */
    this._div = $("div");

    this._order = [];
    It.range(8 + params[0].length).each(
      () => this._order.push(false)
    );
    this._order[FLEA] = true;
    if (isChampions) {
      this._order[DATE_MODEL] = true;
    }
  }

  isAscendant (ix) {
    return this._order[ix];
  }

  setAscendant (ix, value) {
    this._order = [];
    It.range(8 + this._params[0].length).each(
      () => this._order.push(false)
    );
    this._order[FLEA] = true;
    if (this._isChampions) {
      this._order[DATE_MODEL] = true;
    }
    this._order[ix] = value;
  }

  sort (ix) {
    const ascendant = this.isAscendant(ix);
    if (ix < PARAM_COLS) {
      this._entries.sort((r1, r2) => {
        let v1 = r1[ix];
        let v2 = r2[ix];
        if (ix === DATE_MODEL && this._isChampions) {
          v2 = v1 > v2 ? 0 : v1 < v2 ? 2 : 1;
          v1 = 1;
        } else if (ix === DATE_MODEL) {
          v1 = Number(v1);
          v2 = Number(v2);
        } else if (ix === FLEA) {
          if (r1[ix][0] === r2[ix][0]) {
            if (r1[ix][0] === r2[ix][0]) {
              v1 = Number(r1[ix][1]);
              v2 = Number(r2[ix][1]);
            } else {
              v1 = Number(r1[ix][2]);
              v2 = Number(r2[ix][2]);
            }
          } else {
            v1 = Number(r1[ix][0]);
            v2 = Number(r2[ix][0]);
          }
        }
        return v1 !== v2
          ? ascendant ? v1 - v2 : v2 - v1
          : r1[SEL] - r2[SEL]
        ;
      });
    } else {
      this._entries.sort((r1, r2) => {
        const v1 = r1[PARAMS][ix - PARAM_COLS];
        const v2 = r2[PARAMS][ix - PARAM_COLS];
        return v1 !== v2
          ? ascendant ? v1 - v2 : v2 - v1
          : r1[SEL] - r2[SEL]
        ;
      });
    }
    this.setAscendant(ix, !ascendant);
  }

  header () {
    const self = this;
    function td (ix, label) {
      return $("td").klass("header")
        .add(Ui.link(() => {
          self.sort(ix);
          self.showTable();
        }).klass("link").html(label));
    }

    let cols = [
      $("td").klass("header").text(_("Nº")),
      td(FLEA, _("Id")),
      td(ASSETS, _("Assets")),
      td(AVG, _("Pf. Avg")),
      td(MDV, _("Pf. MDV")),
      td(SEL, _("Pf. Sel")),
      td(BUYS, _("Buys")),
      td(SELLS, _("Sells"))
    ];
    if (this._isBests) {
      if (this._isChampions) {
        cols.splice(1, 0, td(DATE_MODEL, _("Model")));
      } else {
        cols.splice(1, 0, td(DATE_MODEL, _("Date")));
      }
    }
    cols = cols.concat(
      this._params[0].map((p, i) => td(PARAM_COLS + i, p))
    );

    return $("tr").adds(cols);
  }

  body () {
    const self = this;
    function cols (e, i) {
      const fleaName = e[FLEA][0] + "-" + e[FLEA][1] + "-" + e[FLEA][2];
      let cs = [
        $("td").klass("header").style("text-align:right").text(String(i + 1)),
        $("td").klass("menu").text(fleaName),
        $("td").klass("fnumber").text(new Dec(e[ASSETS], 2).toEu()),
        $("td").klass("fnumber").text(new Dec(e[AVG] * 100, 2).toEu() + "%"),
        $("td").klass("fnumber").text(new Dec(e[MDV] * 100, 2).toEu() + "%"),
        $("td").klass("fnumber").text(new Dec(e[SEL] * 100, 2).toEu() + "%"),
        $("td").klass("fnumber").text(new Dec(e[BUYS], 0).toEu()),
        $("td").klass("fnumber").text(new Dec(e[SELLS], 0).toEu())
      ];
      let formats = self._params[1];
      let pops = [];

      if (self._isBests) {
        if (self._isChampions) {
          const model = e[DATE_MODEL];
          const ps = self._params[1][model];
          pops = ps[0]; // Parameter names
          formats = ps[1];
          cs.splice(1, 0,
            $("td").klass("menu").add(
              Ui.link(() => self.showCharts(model, pops.length, fleaName))
                .klass("link").html(e[DATE_MODEL])
            )
          );
        } else {
          cs.splice(1, 0, $("td").klass("menu")
            .html(DateDm.fromStr(e[DATE_MODEL]).toString())
          );
        }
      }
      cs = cs.concat(formats.map((p, i) => {
        const r = $("td").klass("param").text(
          p[0] + new Dec(e[PARAMS][i] * p[1], p[2]).toEu() + p[3]
        );
        if (self._isChampions) {
          r.att("title", pops[i]);
        }
        return r;
      }));

      return cs;
    }

    return this._entries.map((e, i) =>
      $("tr").adds(cols(e, i))
    );
  }

  showTable () {
    if (this._entries.length === 0) {
      this._div.removeAll().add($("table").att("align", "center")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data")))));
      return;
    }
    this._div.removeAll().add($("table").att("align", "left").klass("white")
      .add(this.header())
      .adds(this.body())
    );
  }

  async showCharts (model, group, flea) {
    const rq = {
      "module": "fleas",
      "source": "Dtable",
      "rq": "nicks"
    };
    const rp = await Main.client.rq(rq);
    const nicks = rp["nicks"];
    this._div.removeAll().add(
      new Wcharts(Wcharts.CHAMPIONS, model, nicks, group, flea).wg
    );
  }

  /**
   * @return {!Domo}
   */
  wg () {
    if (this._isBests && !this._isChampions) {
      this.sort(DATE_MODEL);
    } else {
      this.sort(SEL);
    }
    this.showTable();
    return this._div;
  }
}

