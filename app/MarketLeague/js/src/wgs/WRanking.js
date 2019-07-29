// Copyright 26-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import {PreviousGroup, LeagueGroup} from "../Data.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Calc from "../Calc.js";
import TRanking from "./TRanking.js";
import WMatches from "./WMatches.js";

const $ = Ui.$;

export default class WRanking {
  /**
   * @param {!PreviousGroup} previous
   * @param {!LeagueGroup} current
   */
  constructor (previous, current) {
    this._previous = previous;
    this._current = current;
  }

  /**
   * @param {!Domo} parent
   * @return {void}
   */
  show (parent) {
    const wg = $("div")
      .add($("div").klass("head").html(_("Standings")))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").klass("header")
            .add($("div").klass("head")
              .add(Ui.link(() =>
                new WMatches(
                  1, this._previous, this._current.first, this.show.bind(this)
                ).show(parent)
              ).setStyle("color", "#000080").html(_("First Division")))))
          .add($("td").klass("header")
            .add($("div").klass("head")
              .add(Ui.link(() =>
                new WMatches(
                  2, this._previous, this._current.second, this.show.bind(this)
                ).show(parent)
              ).setStyle("color", "#000080").html(_("Second Division")))))
          .add($("td").klass("header")
            .add($("div").klass("head")
              .add(Ui.link(() =>
                new WMatches(
                  3, this._previous, this._current.third, this.show.bind(this)
                ).show(parent)
              ).setStyle("color", "#000080").html(_("Third Division"))))))
        .add($("tr")
          .add($("td").klass("border").style("vertical-align:top")
            .add(new TRanking(
              1, this._previous,
              Calc.ranking(this._current.first, this._previous.first.length - 1)
            ).wg)
          ).add($("td").klass("border").style("vertical-align:top")
            .add(new TRanking(
              2, this._previous,
              Calc.ranking(this._current.second, this._previous.second.length - 1)
            ).wg)
          ).add($("td").klass("border").style("vertical-align:top")
            .add(new TRanking(
              3, this._previous,
              Calc.ranking(this._current.third, this._previous.third.length - 1)
            ).wg)
          )));
    parent.removeAll().add(wg);
  }
}
