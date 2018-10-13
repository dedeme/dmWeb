// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Dec from "./dmjs/Dec.js";

const $ = Ui.$;

const MAX_BET = 20000;

const strTh = () => $("td").style(
  "padding-left: 4px;" +
  "padding-right: 4px;" +
  "border : 1px solid rgb(110,130,150);" +
  "background-color : rgb(200, 200, 200);" +
  "text-align:center;"
);

const strTd = () => $("td").style(
  "padding-left: 4px;" +
  "padding-right: 4px;" +
  "border : 1px solid rgb(110,130,150);" +
  "background-color : rgb(250, 250, 250);" +
  "text-align:center;"
);

const numTd = () => $("td").style(
  "padding-left: 8px;" +
  "padding-right: 2px;" +
  "border : 1px solid rgb(110,130,150);" +
  "background-color : rgb(250, 250, 250);" +
  "text-align:right;"
);

const udTd = (ix, value) => {
  switch (ix) {
  case 0: return numTd()
    .att("title", _("Inversion"))
    .text(new Dec(value * MAX_BET, 0).toEu());
  case 1: return numTd()
    .att("title", _("Days"))
    .text(new Dec(2 + value * 200, 0).toEu());
  case 2: return numTd()
    .att("title", _("Buy Strap"))
    .text(new Dec(value * 0.2 * 100, 2).toEu() + "%");
  case 3: return numTd()
    .att("title", _("Sell Strap"))
    .text(new Dec(value * 0.2 * 100, 2).toEu() + "%");
  default: return numTd().text(value);
  }
};

const tbTd = (ix, value) => {
  switch (ix) {
  case 0: return numTd()
    .att("title", _("Inversion"))
    .text(new Dec(value * MAX_BET, 0).toEu());
  case 1: return numTd()
    .att("title", _("Days"))
    .text(new Dec(1 + value * 200, 0).toEu());
  case 2: return numTd()
    .att("title", _("AvgDays"))
    .text(new Dec(1 + value * 200, 0).toEu());
  case 3: return numTd()
    .att("title", _("Buy Strap"))
    .text(new Dec(value * 0.48 * 100, 0).toEu() + "%");
  case 4: return numTd()
    .att("title", _("Sell Strap"))
    .text(new Dec(value * 0.48 * 100, 0).toEu() + "%");
  default: return numTd().text(value);
  }
};

const familyTd = (family, ix, value) => {
  if (value === undefined) {
    return numTd().text("---");
  }
  if (family === "UpDown") {
    return udTd(ix, value);
  }
  if (family === "TopBottom") {
    return tbTd(ix, value);
  }
  return numTd().text(value);
};

/** Data page. */
export default class Data {
  /**
   * @param {!Main} main Main
   * @param {string} family Fleas family
   * @param {*} data
   */
  constructor (main, family, data) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
    /**
     * @private
     * @type {string}
     */
    this._family = family;
    /**
     * @private
     * @type {boolean}
     */
    this._ok = data.length !== 0;
    /**
     * @private
     * @type {*}
     */
    this._data = data;

    this._div = $("div");
  }

  showEmpty (family) {
    this._main.dom.show("data", family, $("div")
      .style("text-align:center")
      .add($("h2").html(_("Data")))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").klass("frame").html(_("Data not found")))))
    );
  }

  showData (cycle, column) {
    const data = this._data;
    const keys = Object.keys(/** @type {!Object} */(data));
    keys.sort((k1, k2) => Number(k2) - Number(k1));

    let bests = data[cycle];
    if (bests === undefined) {
      cycle = keys[0];
      bests = data[cycle];
    }
    bests.sort((a1, a2) => {
      const difPond = a2[1] - a1[1];
      if (column === "id") {
        const d1 = a1[0][0].split("-");
        const d2 = a2[0][0].split("-");
        if (d1[0] === d2[0] && d1[1] === d2[1]) {
          return difPond;
        }
        if (d1[0] === d2[0]) {
          return d1[1] - d2[1];
        }
        return d1[0] - d2[0];
      }
      if (column === "family") {
        const d1 = a1[0][1];
        const d2 = a2[0][1];
        if (d1 === d2) {
          return difPond;
        }
        return d1.localeCompare(d2);
      }
      if (column === "assets") {
        const d1 = a1[2];
        const d2 = a2[2];
        if (d1 === d2) {
          return difPond;
        }
        return d2 - d1;
      }
      if (column === "buy" || column === "sell") {
        const d1 = column === "buy" ? a1[3] : a1[4];
        const d2 = column === "buy" ? a2[3] : a2[4];
        if (d1 === d2) {
          return difPond;
        }
        return d1 - d2;
      }
      return difPond;
    });

    const cyclesDiv = $("div")
      .klass("frame")
      .add($("span").html(_("Cycles")))
      .add($("hr"))
    ;

    for (const k of keys) {
      cyclesDiv
        .add(cycle === k
          ? $("span").html("<b>" + k + "<b>")
          : Ui.link(() => {
            this.showData(k, column);
          })
            .klass("link").html(k))
        .add($("br"));
    }

    const table = $("table").att("align", "center").style(
      "border-collapse : collapse;"
    )
      .add($("tr")
        .add(strTh().att("rowspan", 2).html(_("Nm.")))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "id");
          })
            .klass("link").html(_("Id"))))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "family");
          })
            .klass("link").html(_("Family"))))
        .add(strTh().att("colspan", 5).html(_("Family Data")))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "pond");
          })
            .klass("link").html(_("Pond."))))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "assets");
          })
            .klass("link").html(_("Assets"))))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "buy");
          })
            .klass("link").html(_("Buy"))))
        .add(strTh().att("rowspan", 2)
          .add(Ui.link(() => {
            this.showData(cycle, "sell");
          })
            .klass("link").html(_("Sell")))))
      .add($("tr")
        .add(strTh().html("P. 1"))
        .add(strTh().html("P. 2"))
        .add(strTh().html("P. 3"))
        .add(strTh().html("P. 4"))
        .add(strTh().html("P. 5")))
    ;

    let i = 1;
    for (const row of bests) {
      table.add($("tr")
        .add(numTd().text(String(i++)))
        .add(strTd().text(row[0][0]))
        .add(strTd().text(row[0][1]))
        .add(familyTd(row[0][1], 0, row[0][2][0]))
        .add(familyTd(row[0][1], 1, row[0][2][1]))
        .add(familyTd(row[0][1], 2, row[0][2][2]))
        .add(familyTd(row[0][1], 3, row[0][2][3]))
        .add(familyTd(row[0][1], 4, row[0][2][4]))
        .add(numTd().text(Dec.newStr(row[1], 2).toEu()))
        .add(numTd().text(Dec.newStr(row[2], 2).toEu()))
        .add(numTd().text(row[3]))
        .add(numTd().text(row[4])));
    }

    this._div.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .style("vertical-align: top;align:center;width:70px;")
            .add(cyclesDiv))
          .add($("td")
            .add(table))))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    const family = this._family;

    if (!this._ok) {
      this.showEmpty(family);
      return;
    }

    this._main.dom.show("data", family, $("div")
      .style("text-align:center")
      .add($("h2").html(_("Data")))
      .add(this._div)
    );

    this.showData("", "pond");
  }
}

