// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../../dmjs/Ui.js";
import It from "../../../../dmjs/It.js";

const $ = e => Ui.$(e);

function formatN (n) {
  if (n < 10) return "0" + String(n);
  return String(n);
}

/**
    Whour widget.
**/
export default class Whour {

  /**
      @param {number} hour
      @param {number} minute
      @param {function():void} action
  **/
  constructor (hour, minute, action) {
    const hlist = [...It.range(24)].map(
      n => (n === hour ? "+" : "") + formatN(n)
    );

    const mlist = [...It.range(12)].map(
      n => (n * 5 === minute ? "+" : "") + formatN(n * 5)
    );


    this._h = Ui.select("wh", hlist).on("change", () => action());
    this._m = Ui.select("wh", mlist).on("change", () => action());
  }

  /**
      @return {number}
  **/
  get hour () {
    return this._h.e.selectedIndex;
  }

  /**
      @return {number}
  **/
  get minute () {
    return this._m.e.selectedIndex * 5;
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return $("table")
      .add($("tr")
        .add($("td")
          .add(this._h))
        .add($("td")
          .add($("span")
            .html("<big> : </big>")))
        .add($("td")
          .add(this._m)))
    ;
  }
}
