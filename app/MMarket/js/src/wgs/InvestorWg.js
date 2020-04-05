// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line
import Dec from "../dmjs/Dec.js"; // eslint-disable-line
import Investor from "../data/flea/Investor.js"; // eslint-disable-line
import {_} from "../I18n.js";

const $ = e => Ui.$(e);

/**
    Log widget.
**/
export default class InvestorWg {
  /**
      @param {!Investor} investor
  **/
  constructor (investor) {
    this._investor = investor;

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  view () {
    const model = this._investor.model;
    const eflea = this._investor.eflea;

    this._wg.removeAll()
      .add($("div").klass("head").html(model.name + "<br>" + eflea.flea.name))
      .add($("div").klass("separator"))
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .adds(model.parNames.map(e => $("td").klass("headGrey").text(e))))
        .add($("tr")
          .adds(eflea.flea.gen.map((e, i) =>
            $("td").klass("number")
              .text(
                eval(model.parJsFmt[i].split("#N#").join(String(e)))
              )))))
      .add($("div").klass("separator"))
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td").klass("headGrey").text(_("Assets")))
          .add($("td").klass("headGrey").text(_("Profits")))
          .add($("td").klass("headGrey").text(_("Points")))
          .add($("td").klass("headGrey").text(_("Buys")))
          .add($("td").klass("headGrey").text(_("Sells"))))
        .add($("tr")
          .add($("td").klass("number")
            .text(new Dec(eflea.assets, 2).toIso()))
          .add($("td").klass("number")
            .text(new Dec(eflea.profits, 4).toIso()))
          .add($("td").klass("number")
            .text(new Dec(eflea.ev, 4).toIso()))
          .add($("td").klass("number2")
            .text(new Dec(eflea.buys, 0).toIso()))
          .add($("td").klass("number2")
            .text(new Dec(eflea.sells, 0).toIso()))))
    ;
  }
}
