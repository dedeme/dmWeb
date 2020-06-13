// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line
import Param from "./Param.js";

const $ = e => Ui.$(e);

/**
    Params widget.
**/
export default class Params {
  /**
      @param {!Array<string>} names Parameter names.
      @param {!Array<number>} mins Minimum values
      @param {!Array<number>} maxs Maximum values
      @param {string} id TextFields root id. Final id will be made with
                         id + index.
      @param {string} nextId Id of next widget to pass focus.
      @param {!Array<number>=} values Default values.
  **/
  constructor (names, mins, maxs, id, nextId, values) {
    this._ps = [];
    for (let i = 0; i < mins.length; ++i) {
      const nx = (i < mins.length - 1) ? id + String(i + 1) : nextId;
      this._ps.push(values === undefined
        ? new Param(names[i], mins[i], maxs[i], id + String(i), nx)
        : new Param(names[i], mins[i], maxs[i], id + String(i), nx, values[i])
      );
    }

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
      @return {!Array<number>}
  **/
  get value () {
    return this._ps.map(p => p.value);
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg.removeAll()
      .add($("table").klass("frame")
        .add($("tr")
          .adds(this._ps.map(p => $("td").add(p.wg)))))
    ;
  }

}
