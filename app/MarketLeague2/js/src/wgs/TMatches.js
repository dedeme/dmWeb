// Copyright 26-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import {Mark} from "../Calc.js"; //eslint-disable-line
import {RoundResult, Match} from "../Data.js"; //eslint-disable-line

const $ = Ui.$;

export default class TMaches {
  /**
   * @param {!Array<string>} nicks
   * @param {!Array<!Match>} matches
   * @param {!RoundResult} roundResult
   */
  constructor (nicks, matches, roundResult) {
    this._nicks = nicks;
    this._matches = matches;
    this._results = roundResult.results;
  }

  /** @return {!Domo} */
  get wg () {
    const padding = "padding-left: 4px;padding-right: 4px;";
    const fmt = padding + "border-left: 1px solid rgb(110,130,150);";
    let restingNick = null;
    const rows = [];
    for (let i = 0; i < this._matches.length; ++i) {
      const match = this._matches[i];
      const rs = this._results[i];

      let sign = null;
      if (rs.result === 0) sign = "X";
      else if (rs.result > 0) sign = String(rs.result);
      else {
        restingNick = match.up < match.down
          ? this._nicks[match.up]
          : this._nicks[match.down];
      }

      if (sign !== null) {
        rows.push($("tr")
          .add($("td").style(fmt).klass("nick")
            .html(this._nicks[match.up]))
          .add($("td").style(fmt).klass("nick")
            .html(this._nicks[match.down]))
          .add($("td").style(fmt + "text-aling:right").html(sign))
          .add($("td").style(fmt + "text-aling:right")
            .html(new Dec(rs.dif, 4).toEu()))
        );
      }
    }
    return $("table").klass("border").style("border-collapse : collapse;")
      .adds(rows);
  }
}
