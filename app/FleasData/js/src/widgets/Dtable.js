// Copyright 27-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import DateDm from "../dmjs/DateDm.js";
import Dec from "../dmjs/Dec.js";
import It from "../dmjs/It.js";
import {_} from "../I18n.js";
const $ = Ui.$;

const DATE = 0;
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
   * @param {!Array<?>} params Flea model parameters data.
   * @param {!Array<?>} entries Table elements.
   */
  constructor (isBests, params, entries) {
    /**
     * @private
     * @type {boolean}
     */
    this._isBests = isBests;

    /**
     * @private
     * @type {!Array<?>}
     */
    this._params = params;

    /**
     * @private
     * @type {!Array<?>}
     */
    this._entries = entries;

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
    this._order[ix] = value;
  }

  sort (ix) {
    const ascendant = this.isAscendant(ix);
    if (ix < PARAM_COLS) {
      this._entries.sort((r1, r2) => {
        let v1 = r1[ix];
        let v2 = r2[ix];
        if (ix === DATE) {
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
      cols.splice(1, 0, td(DATE, _("Date")));
    }
    cols = cols.concat(
      this._params[0].map((p, i) => td(PARAM_COLS + i, p))
    );

    return $("tr").adds(cols);
  }

  body () {
    const self = this;
    function cols (e, i) {
      let cs = [
        $("td").klass("header").style("text-align:right").text(String(i + 1)),
        $("td").klass("menu").text(
          e[FLEA][0] + "-" + e[FLEA][1] + "-" + e[FLEA][2]
        ),
        $("td").klass("number").text(new Dec(e[ASSETS], 2).toEu()),
        $("td").klass("number").text(new Dec(e[AVG] * 100, 2).toEu() + "%"),
        $("td").klass("number").text(new Dec(e[MDV] * 100, 2).toEu() + "%"),
        $("td").klass("number").text(new Dec(e[SEL] * 100, 2).toEu() + "%"),
        $("td").klass("number").text(new Dec(e[BUYS], 0).toEu()),
        $("td").klass("number").text(new Dec(e[SELLS], 0).toEu())
      ];
      if (self._isBests) {
        cs.splice(1, 0, $("td").klass("menu")
          .html(DateDm.fromStr(e[DATE]).toString())
        );
      }
      cs = cs.concat(self._params[1].map((p, i) =>
        $("td").klass("param").text(
          p[0] + new Dec(e[PARAMS][i] * p[1], p[2]).toEu() + p[3]
        )
      ));

      return cs;
    }

    return this._entries.map((e, i) =>
      $("tr").adds(cols(e, i))
    );
  }

  showTable () {
    if (this._entries.length === 0) {
      this._div.removeAll().add($("table").att("align", "left")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data")))));
      return;
    }
    this._div.removeAll().add($("table").att("align", "left").klass("data")
      .add(this.header())
      .adds(this.body())
    );
  }

  /**
   * @return {!Domo}
   */
  wg () {
    if (this._isBests) {
      this.sort(DATE);
    } else {
      this.sort(SEL);
    }
    this.showTable();
    return this._div;
  }
}

