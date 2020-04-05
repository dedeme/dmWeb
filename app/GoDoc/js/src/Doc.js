// Copyright 03-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";

const $ = e => Ui.$(e);

/**
    Page to show documentation
**/
export default class Doc {
  /**
      @param {string} lib
  **/
  constructor (lib) {
    const path = lib.charAt(0) === "*"
      ? "http://localhost:6061/pkg/" + lib.substring(1)
      : "http://localhost:6060/pkg/" + lib
    ;

    this._iframe = $("iframe")
      .att("id", "iframe")
      .att("src", path)
      .att("width", window.innerWidth - 25)
      .att("height", window.innerHeight - 70)
    ;
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

  /**
      @private
  **/
  view () {
    this._wg.removeAll().add(this._iframe);
    window.onresize = () => { this.resize() };
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  resize () {
    this._iframe
      .att("width", window.innerWidth - 30)
      .att("height", window.innerHeight - 60)
    ;
  }
}

