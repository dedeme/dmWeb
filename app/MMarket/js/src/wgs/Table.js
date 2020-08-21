// Copyright 29-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Table for show data.
**/

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import DateDm from "../dmjs/DateDm.js";
import {_} from "../I18n.js";
import Cts from "../data/Cts.js";

const $ = e => Ui.$(e);

/**
    Column description.
    The field format expect next values:
         <0: String format.
          4: Percentage format (new Dec(v*100, 2).toIso() + "%").
      other: Number format (new Dec(v, 'other').toIso()).
**/
export class Col {
  /* .
  _rc_ Col
    title : string
    type  : number
    # See overview.
    format: number
    # If launch an action.
    link  : boolean
    # If its ascendent order.
    +asc   : boolean
  */

  /*--*/
  /**
      @param {string} title
      @param {number} type
      @param {number} format See overview.
      @param {boolean} link If launch an action.
      @param {boolean} asc If its ascendent order.
  **/
  constructor (
    title,
    type,
    format,
    link,
    asc
  ) {

    /**
        @private
        @type {string}
    **/
    this._title = title;

    /**
        @private
        @type {number}
    **/
    this._type = type;

    /**
        @private
        @type {number}
    **/
    this._format = format;

    /**
        @private
        @type {boolean}
    **/
    this._link = link;

    /**
        @private
        @type {boolean}
    **/
    this._asc = asc;

  }

  /**
      @return {string}
  **/
  get title () {
    return this._title;
  }

  /**
      @return {number}
  **/
  get type () {
    return this._type;
  }

  /**
      @return {number}
  **/
  get format () {
    return this._format;
  }

  /**
      @return {boolean}
  **/
  get link () {
    return this._link;
  }

  /**
      @return {boolean}
  **/
  get asc () {
    return this._asc;
  }

  /**
      @param {boolean} value
   * @return void
  **/
  setAsc (value) {
    this._asc = value;
  }
  /*--*/

  static get NUMBER () { return 0 }

  static get PARAM () { return 1 }

  static get STRING () { return 2 }

  static get P_STRING () { return 3 }

  static get COUNTER () { return 4 }

  static get ICON () { return 5 }

  static get DATE () { return 6 }
}

/**
    Table for show data.
**/
export class Table {
  /**
      @param {!Domo} wg
      @param {!Array<!Col>} cols
      @param {!Array<Array<?>>} table
      @param {number} orderCol Number of main column for ordering or -1 if
                      table is not reorderable.
      @param {function(?, number):string} link
  **/
  constructor (wg, cols, table, orderCol, link) {
    this._wg = wg;
    this._cols = cols;
    this._table = table;
    this._orderCol = orderCol;
    this._link = link;
    this._mainOrder = orderCol === -1 ? false : cols[orderCol - 1].asc;

    if (orderCol !== -1) this.sortBy(this._orderCol);
    else this.view();
  }

  // View ----------------------------------------------------------------------

  view () {
    const cols = this._cols;

    if (this._table.length === 0) {
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

    const header = cols.map((c, i) =>
      c.type === Col.COUNTER
        ? $("td")
          .klass("header")
          .text(c.title)
        : $("td")
          .klass("header")
          .add(this._orderCol === -1
            ? $("span").text(c.title)
            : Ui.link(() => this.sortBy(i + 1))
              .klass("link")
              .text(c.title))
    );

    const rows = this._table.map((r, iRow) => {
      const element = r[0];
      const r2 = [...It.from(r).drop(1)];
      return $("tr")
        .adds(r2.map((v, i) => {
          const c = cols[i];
          const t = c.type;
          return t === Col.COUNTER
            ? $("td")
              .klass("header")
              .style("text-align:right")
              .text(new Dec(iRow, 0).toIso())
            : t === Col.ICON
              ? c.link
                ? $("td")
                  .klass("menu")
                  .add($("a")
                    .att("href", this._link(element, i))
                    .add(Ui.img(v)))
                : $("td")
                  .klass("menu")
                  .add(Ui.img(v))
              : t === Col.NUMBER || t === Col.PARAM
                ? c.link
                  ? $("td")
                    .klass(t === Col.NUMBER ? "fnumber" : "fparam")
                    .add($("a")
                      .att("href", this._link(element, i))
                      .text(Cts.nformat(v, c.format)))
                  : $("td")
                    .klass(t === Col.NUMBER ? "fnumber" : "fparam")
                    .text(Cts.nformat(v, c.format))
                : t === Col.DATE
                  ? $("td")
                    .klass("menu")
                    .text(DateDm.fromStr(v).toString())
                  : c.link
                    ? $("td")
                      .klass(t === Col.STRING ? "menu" : "fparam")
                      .add($("a")
                        .att("href", this._link(element, i))
                        .text(v))
                    : $("td")
                      .klass(t === Col.STRING ? "menu" : "fparam")
                      .text(v)
          ;
        }));
    });

    this._wg
      .removeAll()
      .add($("table").klass("white")
        .add($("tr")
          .adds(header))
        .adds(rows))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param{number} iCol Row-column index. this._cols has 'iCol - 1' index.
  **/
  sortBy (iCol) {
    const cols = this._cols;
    if (iCol === this._orderCol) {
      this._table.sort((r1, r2) =>
        cols[iCol - 1].asc
          ? r1[iCol] > r2[iCol] ? 1 : -1
          : r1[iCol] < r2[iCol] ? 1 : -1
      );
      cols[iCol - 1].setAsc(!cols[iCol - 1].asc);
    } else {
      this._table.sort((r1, r2) =>
        cols[iCol - 1].asc
          ? r1[iCol] > r2[iCol]
            ? 1
            : r1[iCol] < r2[iCol]
              ? -1
              : this._mainOrder
                ? r1[this._orderCol] > r2[this._orderCol] ? 1 : -1
                : r1[this._orderCol] < r2[this._orderCol] ? 1 : -1
          : r1[iCol] < r2[iCol] ? 1
            : r1[iCol] > r2[iCol]
              ? -1
              : this._mainOrder
                ? r1[this._orderCol] > r2[this._orderCol] ? 1 : -1
                : r1[this._orderCol] < r2[this._orderCol] ? 1 : -1
      );
      cols[iCol - 1].setAsc(!cols[iCol - 1].asc);
    }
    this.view();
  }
}
