// Copyright 11-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting profits.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
import {_} from "../../I18n.js";
import Cts from "../../data/Cts.js";
import Broker from "../../data/Broker.js";

const $ = e => Ui.$(e);

function invTd (invs) {
  return $("td")
    .klass("borderWhite")
    .text(invs.map(i => String(i)).join(" - "))
  ;
}

function ciaTd (nick) {
  return $("td")
    .klass("borderWhite")
    .style("text-align:left")
    .text(nick)
  ;
}

function stocksTd (stock) {
  return $("td")
    .klass("borderWhite")
    .style("text-align:right")
    .text(new Dec(stock, 0).toIso())
  ;
}

/**
    Accounting profits.
**/
export default class Trading {
  /**
      @param {!Domo} wg
      @param {number} bet
      @param {!Array<?>} operations Has the following structure:
        [
          [toBuy: boolean, toSell:number, investors:[number, ...], nick:string],
          [toBuy: boolean, toSell:number, investors:[number, ...], nick:string],
          ... (unsorted)
          until N inv-companies
        ]
        NOTE: If an investor should buy toBuy is true and if it should sell
              toSell has a number > 0. An operation can have sells and buys
              at the same time.
  **/
  constructor (wg, bet, operations) {
    this._wg = wg;
    this._bet = bet;
    this._operations = operations;

    this._bentry = Ui.field("pentry").value(new Dec(bet).toIso());
    Ui.changePoint(this._bentry);
    this._pentry = Ui.field("calcBt").att("id", "pentry");
    Ui.changePoint(this._pentry);
    this._result = $("div").klass("frame").style("text-align:right");

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const os = this._operations;
    const buys = os.filter(o => o[0]);
    const buyTrs = buys.length === 0
      ? [
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .klass("borderWhite")
            .html(_("Without operations")))
      ]
      : buys.map(o =>
        $("tr")
          .add(invTd(o[2]))
          .add(ciaTd(o[3]))
      )
    ;
    const sells = os.filter(o => o[1] > 0);
    const sellTrs = sells.length === 0
      ? [
        $("tr")
          .add($("td")
            .att("colspan", 3)
            .klass("borderWhite")
            .html(_("Without operations")))
      ]
      : sells.map(o =>
        $("tr")
          .add(invTd(o[2]))
          .add(ciaTd(o[3]))
          .add(stocksTd(o[1]))
      )
    ;

    this._wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("vertical-align:top;width:5px")
            .add($("table")
              .klass("frame4")
              .add($("tr")
                .add($("td")
                  .add($("div")
                    .klass("head")
                    .text(_("Buy")))))
              .add($("tr")
                .add($("td")
                  .add($("hr"))))
              .add($("tr")
                .add($("td")
                  .style("text-align:center")
                  .add($("div")
                    .html("<b>" + _("Invest") + "</b>"))))
              .add($("tr")
                .add($("td")
                  .add(this._bentry)))
              .add($("tr")
                .add($("td")
                  .style("text-align:center")
                  .add($("div")
                    .html("<b>" + _("Price") + "</b>"))))
              .add($("tr")
                .add($("td")
                  .add(this._pentry)))
              .add($("tr")
                .add($("td")
                  .style("text-align:center")
                  .add($("button")
                    .text(_("Calculate"))
                    .on("click", () => this.calculate()))))
              .add($("tr")
                .add($("td")
                  .add($("hr"))))
              .add($("tr")
                .add($("td")
                  .style("text-align:center")
                  .add($("div")
                    .html("<b>" + _("Stocks") + "</b>"))))
              .add($("tr")
                .add($("td")
                  .add(this._result)))))
          .add($("td")
            .style("text-align:center")
            .add($("div")
              .klass("head")
              .html(_("Buys")))
            .add($("table")
              .att("align", "center")
              .klass("buys")
              .add($("tr")
                .add($("td").klass("head").html(_("Inv")))
                .add($("td").klass("head").html(_("Co."))))
              .adds(buyTrs))
            .add($("div")
              .klass("head")
              .html(_("Sells")))
            .add($("table")
              .att("align", "center")
              .klass("sells")
              .add($("tr")
                .add($("td").klass("head").html(_("Inv")))
                .add($("td").klass("head").html(_("Co.")))
                .add($("td").klass("head").html(_("Stocks"))))
              .adds(sellTrs)))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  calculate () {
    const bs = this._bentry.value();
    const ps = this._pentry.value();
    if (!Dec.isNumberIso(bs)) {
      alert(`${bs} is not a valid number.`);
      return;
    }
    if (!Dec.isNumberIso(ps)) {
      alert(`${ps} is not a valid number.`);
      return;
    }
    const b = Dec.newIso(bs, 2).value;
    const p = Dec.newIso(ps, 4).value;
    if (p === 0) {
      alert("Price is 0");
      return;
    }
    let rs = (b / p)|0;
    while (rs > 0 && Broker.buy(p, rs) > b) --rs;

    this._result.text(new Dec(rs, 0).toIso());
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return !Promise<!Trading>
  **/
  static async mk (wg) {
    wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("text-align:center")
            .add(Ui.$("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;
    const rp = await Cts.client.send({
      "module": "acc",
      "source": "trading",
      "rq": "idata"
    });

    const /** number */ bet = rp["bet"];
    const /** !Array<?> */ operations = rp["operations"];

    return new Trading(wg, bet, operations);
  }

}
