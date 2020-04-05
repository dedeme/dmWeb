// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line

const $ = e => Ui.$(e);

/**
    Wrule widget.
**/
export default class Wrule {

  /**
      @private
  **/
  static mk (width, color, title) {
    return $("table").klass("main").style(`color:${color}`)
      .add($("tr")
        .add($("td").style(`width:${String(width)}px;`).add($("hr")))
        .add($("td").style("width:5px;white-space: nowrap;").html(title))
        .add($("td").add($("hr"))))
    ;
  }

  /**
      @param {string} title
      @return {!Domo}
  **/
  static mkBig (title) {
    return Wrule.mk(50, "#101010", title);
  }

  /**
      @param {string} title
      @return {!Domo}
  **/
  static mkSmall (title) {
    return Wrule.mk(20, "#808080", title);
  }
}
