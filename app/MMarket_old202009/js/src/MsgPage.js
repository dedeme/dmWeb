// Copyright 21-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line

const $ = e => Ui.$(e);

/**
    Message page.
**/
export default class MsgPage {

  /**
      @param {string} app Application name.
      @param {string} msg Message to show.
      @param {boolean=} withReload If a reload message is shown.
  **/
  constructor (app, msg, withReload = false) {
    this._app = app;
    this._msg = msg;
    this._withReload = withReload;
    this._wg = $("div");
    this.view();
  }

  /**
      Page container.
      @return !Domo
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    const reload = `
      <p><b>
      ${_args(_("Click %0 to continue."), "<a href=''>" + _("here") + "</a>")}
      </b></p>`;
    this._wg
      .add($("div")
        .klass("head")
        .style("padding-bottom:20px;")
        .text(this._app))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add($("table")
              .klass("border")
              .att("width", "100%")
              .style("background-color: #f8f8f8; border-collapse: collapse")
              .add($("tr")
                .add($("td")
                  .style("padding:0px 10px 0px 10px;")
                  .html(`<p>${this._msg}<p>${this._withReload ? reload : ""}`)
                ))))));
  }
}
