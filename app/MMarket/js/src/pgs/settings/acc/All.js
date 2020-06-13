// Copyright 18-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting/All the investors page.
**/

import Maybe from "../../../dmjs/Maybe.js";
import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import {Menu} from "../../../dmjs/Menu.js";
import Dec from "../../../dmjs/Dec.js";
import {_} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Annotations from "./wgs/Annotations.js";

const $ = e => Ui.$(e);

export default class All {
  /**
    @param {!Domo} wg
    @param {!Array<string>} years
    @param {!Array<?>} anns
    @param {number} cash
  **/
  constructor (wg, years, anns, cash) {
    this._wg = wg;
    this._years = years;
    this._year = years[0];
    this._anns = anns;
    this._cash = cash;

    this._body = $("div");
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
    @private
    @return void
  **/
  view () {
    const lopts = [];
    this._years.forEach((year, i) => {
      if (i > 0) {
        lopts.push(Menu.separator());
      }
      lopts.push(Menu.toption(year, year, () => this.year(year)));
    });
    const menu = new Menu(lopts, [], this._year);

    const annsWg = $("div");
    // eslint-disable-next-line
    new Annotations(annsWg, this._anns, Maybe.nothing);

    this._wg
      .removeAll()
      .add(menu.wg)
      .add($("div")
        .add($("div")
          .klass("head")
          .html(_("Annotations")))
        .add($("table")
          .att("align", "center")
          .klass("frame3")
          .add($("tr")
            .add($("td")
              .add($("table").att("align", "right")
                .add($("tr")
                  .add($("td")
                    .klass("rlabel")
                    .add($("span")
                      .html(_("Cash:"))))
                  .add($("td")
                    .klass("number")
                    .text(new Dec(this._cash, 2).toIso()))
                  .add($("td"))))))
          .add($("tr")
            .add($("td").klass("frame")
              .add(annsWg)))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
    @private
    @param {string} y Year to show.
  **/
  async year (y) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "acc/all",
      "rq": "anns",
      "year": y
    });
    this._anns = rp["anns"];
    this._cash = rp["cash"];
    this._year = y;
    this.view();
  }

}
