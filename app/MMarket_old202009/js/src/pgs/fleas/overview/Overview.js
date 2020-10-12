// Copyright 21-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Doc from "../../../data/flea/Doc.js";
import Model from "../../../data/flea/Fmodel.js";  //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Fleas overview page.
**/
export default class Overview {
  /**
      @param {!Domo} wg
      @param {!Model} model
  **/
  constructor (wg, model) {
    this._model = model;
    this._wg = wg;
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const md = this._model;
    this._wg
      .removeAll()
      .add($("div")
        .klass("head")
        .html("<big>" + md.id + "</big><br>" + md.name))
      .add($("table")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .add($("div")
              .klass("frame")
              .style("width:680px")
              .html(Doc.read(md.id))))))
    ;
  }
}


