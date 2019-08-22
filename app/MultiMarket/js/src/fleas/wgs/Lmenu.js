// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";

const $ = e => Ui.$(e);

/** Left menu widget. */
export default class Lmenu {
  /**
   * @param {!Array<string>} entries Menu elements.
   * @param {function(string):Promise<?>} callBack Response to click entries.
   * @param {string} selected Element selected
   */
  constructor (entries, callBack, selected) {
    /**
     * @private
     * @type {!Array<!Domo>}
     */
    this._tds = entries.map(e =>
      $("td").att("id", e)
        .style("cursor:pointer;white-space:nowrap;")
        .on("click", (() => {
          this.markSelected(e);
          callBack(e);
        }).bind(this))
        .html(e));

    /**
     * @private
     * @type {string}
     */
    this._selected = selected;
  }

  /** @private */
  markSelected (model) {
    this._tds.forEach(r => {
      if (r.att("id") === model) {
        r.setStyle("font-weight", "bold");
      } else {
        r.setStyle("font-weight", "normal");
      }
    });
  }

  /**
   * @return {!Domo}
   */
  get wg () {
    this.markSelected(this._selected);
    return $("div").klass("frame")
      .add($("table").klass("main")
        .adds(this._tds.map(td => $("tr").add(td))));
  }
}

