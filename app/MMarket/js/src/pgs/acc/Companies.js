// Copyright 12-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Companies accounting charts.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {_} from "../../I18n.js";
import Cts from "../../data/Cts.js";
import Chart from "./wgs/Chart.js";

const $ = e => Ui.$(e);

/**
    Companies accounting charts.
**/
export default class Companies {
  /**
      @param {!Domo} wg
      @param {!Array<!Array<?>>} cos Selected companies with the following
        structure:
          [
            [nick:string, inPortfolio: boolean, url:string]
            [nick:string, inPortfolio: boolean, url:string]
            ...
          ]
  **/
  constructor (wg, cos) {
    this._wg = wg;
    this._cos = cos;
    this._showAll = false;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const ls = (this._showAll ? this._cos : this._cos.filter(e => e[1])).map(
      e => [e[0], e[2]]
    ).sort((e1, e2) => e1[0] > e2[0] ? 1 : -1);

    function separator () {
      return $("tr")
        .add($("td")
          .att("colspan", 3)
          .html("<hr>"))
      ;
    }

    const chs = $("table")
      .att("align", "center")
      .klass("frame");

    const n = ls.length;
    let tr = $("tr");
    for (let i = 0; i < n; ++i) {
      const chart = Chart.mk(ls[i][0], ls[i][1]);

      switch (i % 3) {
      case 0:
        chs.add(separator());
        tr = $("tr");
        tr.add($("td").add(chart));
        break;
      case 2:
        tr.add($("td").add(chart));
        chs.add(tr);
        break;
      default:
        tr.add($("td").add(chart));
      }
    }

    switch (n % 3) {
    case 1: chs.add(tr.add($("td")).add($("td"))); break;
    case 2: chs.add(tr.add($("td"))); break;
    }
    chs.add(separator());

    this._wg
      .removeAll()
      .add($("div")
        .style("text-align:center")
        .add(Ui.link(() => { this.changeShowAll() })
          .klass("link")
          .html(this._showAll ? _("Portfolio") : _("All Companies"))))
      .add(chs)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  changeShowAll () {
    this._showAll = !this._showAll;
    this.view();
  }



  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return !Promise<!Companies>
  **/
  static async mk (wg) {
    const rp = await Cts.client.send({
      "module": "acc",
      "source": "companies",
      "rq": "list"
    });
    const /** !Array<!Array<?>> */ list = rp["list"];

    return new Companies(wg, list);
  }

}
