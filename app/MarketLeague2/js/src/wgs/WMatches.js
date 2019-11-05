// Copyright 26-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import {League, PreviousGroup} from "../Data.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Calc from "../Calc.js";
import TRanking from "./TRanking.js";
import TMatches from "./TMatches.js";

const $ = Ui.$;

export default class WMatches {
  /**
   * @param {number} division
   * @param {!PreviousGroup} previous
   * @param {!League} league
   * @param {function(!Domo):void} goBack
   */
  constructor (division, previous, league, goBack) {
    this._division = division;
    this._previous = previous;
    this._league = league;
    this._goBack = goBack;

    this._dayDiv = $("div");
  }

  /**
   * @param {!Domo} parent
   * @return {void}
   */
  show (parent) {
    const ndiv = this._division === 1 ? _("First")
      : this._division === 2 ? _("Second") : _("Third");
    const wg = $("div")
      .add($("div").add($("table").klass("frame").att("align", "center")
        .add($("tr").add($("td").add($("div").klass("head")
          .add(Ui.link(() => this._goBack(parent)).setStyle("color", "#000080")
            .html(_("Standings"))))))))
      .add($("div").klass("head").html(_args(_("%0 Division Matches"), ndiv)))
      .add($("table").att("align", "center")
        .add($("tr")
          .adds([...It.range(this._league.results.length)].map(i => {
            return $("td").klass("header").style("width:30px")
              .add($("div").klass("head")
                .add(Ui.link(() => this.day(i + 1)).setStyle("color", "#000080")
                  .html(String(i + 1))));
          }))))
      .add(this._dayDiv)
    ;
    parent.removeAll().add(wg);
  }

  /**
   * @private
   * @param {number} n Number round (first round is 1)
   * @return {void}
   */
  day (n) {
    this._dayDiv.removeAll()
      .add($("div").klass("head").html(_args(_("Round %0"), String(n))))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").klass("header")
            .add($("div").klass("head").html(_("Matches"))))
          .add($("td").klass("header")
            .add($("div").klass("head").html(_("Standings")))))
        .add($("tr")
          .add($("td").klass("border").style("vertical-align:top")
            .add(new TMatches(
              this._league.nicks,
              Calc.rounds(this._league.nicks.length)[n - 1],
              this._league.results[n - 1]
            ).wg))
          .add($("td").klass("border").style("vertical-align:top")
            .add(new TRanking(
              this._division, this._previous,
              Calc.ranking(this._league, n)
            ).wg))))
    ;
  }
}
