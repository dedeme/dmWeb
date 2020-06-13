// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Dec from "../dmjs/Dec.js";
import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line

const $ = e => Ui.$(e);

/**
    Param widget.
**/
export default class Param {
  /**
      @param {string} name Name.
      @param {number} min Minimum value.
      @param {number} max Maximum value.
      @param {string} id TextField id.
      @param {string} nextId Id of next widget to pass focus.
      @param {number=} value Default value
  **/
  constructor (name, min, max, id, nextId, value) {
    this._name = name;
    this._min = new Dec(min, 6);
    this._max = new Dec(max, 6);
    this._nextId = nextId;

    this._inp = Ui.field(this._nextId)
      .att("id", id)
      .style("width:60px")
      .on("change", () => this.onChange())
      .value(value === undefined
        ? new Dec((min + max) / 2, 6).toIso()
        : new Dec(value, 6).toIso()
      )
    ;
    Ui.changePoint(this._inp);
    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {number}
  **/
  get value () {
    const v = this._inp.value().replace(".", "").replace(",", ".");
    if (isNaN(v)) return this._min.value;
    return Number(v);
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    function label () {
      return $("div").style(
        "text-align:center;color:#c9c9c9;font-style:italic"
      );
    }
    this._wg.removeAll()
      .add($("div").style("text-align:center").text(this._name))
      .add(label().text(this._min.toIso()))
      .add(this._inp)
      .add(label().text(this._max.toIso()))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  onChange () {
    const v = this.value;
    if (v < this._min.value) this._inp.value(this._min.toIso());
    if (v > this._max.value) this._inp.value(this._max.toIso());
    this._inp.value(new Dec(v, 6).toIso());
  }

}
