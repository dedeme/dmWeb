// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line

const $ = e => Ui.$(e);

/**
    Expired page.
**/
export default class Expired {

  /**
      @param {string} app Application name
  **/
  constructor (app) {
    this._app = app;
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
      @return {void}
  **/
  view () {
    this._wg
      .add($("p").att("style", "text-align:center")
        .html(`<big><b>${this._app}</b></big>`))
      .add($("table").att("class", "main")
        .add($("tr")
          .add($("td")
            .add($("table")
              .att("class", "border")
              .att("width", "100%")
              .att("style",
                "background-color: #f8f8f8;" +
                "border-collapse: collapse;")
              .add($("tr")
                .add($("td")
                  .att("style", "padding:0px 10px 0px 10px;")
                  .html("<p>" + _("Session is expired.") + "<p>" +
                    "<p><b>" +
                    _args(_("Click %0 to continue."),
                      "<a href=''>" + _("here") + "</a>"
                    ) + "</b></p>"
                  )))))));
  }

}
