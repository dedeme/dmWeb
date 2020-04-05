// Copyright 10-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Fleas Overview Tests page.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Doc from "../../data/flea/Doc.js";
import Model from "../../data/flea/Fmodel.js";  //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Fleas overview tests page.
**/
export default class Overview {
  /**
      @param {!Model} model
  **/
  constructor (model) {
    this._model = model;
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
    const md = this._model;
    this._wg.removeAll()
      .add($("div").klass("head")
        .html("<big>" + md.id + "</big><br>" + md.name))
      .add($("table").att("align", "center").add($("tr").add($("td")
        .add($("div").klass("frame").style("width:600px")
          .html(Doc.read(md.id))))))
    ;
  }
}


