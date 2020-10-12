// Copyright 01-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import {_} from "../../../I18n.js";
import Server from "../../../data/Server.js"; //eslint-disable-line
import Servers from "./Servers.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Modify server names page.
**/
export default class Names {
  /**
      @param {!Domo} wg
      @param {!Servers} serversPg
      @param {!Server} server
  **/
  constructor (wg, serversPg, server) {
    this._wg = wg;
    this._serversPg = serversPg;
    this._server = server;

    this._shortNameField = Ui.field("nameField")
      .value(this._server.shortName);
    this._nameField = Ui.field("btAccept")
      .att("id", "nameField")
      .value(this._server.name);

    this.view();
  }

  // View ----------------------------------------------------------------------
  view () {
    const sv = this._server;

    this._wg
      .removeAll()
      .style("text-align:center")
      .add($("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add($("table")
        .att("align", "center")
        .style(`border-top: 1px solid rgb(110,130,150);
                border-bottom: 1px solid rgb(110,130,150)`)
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("Short Name") + ":"))
          .add($("td")
            .add(this._shortNameField)))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("Name") + ":"))
          .add($("td")
            .add(this._nameField)))
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style("padding-top:4px")
            .add($("button").text(_("Reset"))
              .on("click", () => { this.reset() }))
            .add($("span")
              .text(" "))
            .add($("button")
              .text(_("Modify"))
              .on("click", () => { this.modify() })))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  reset () {
    new Names(this._wg, this._serversPg, this._server); //eslint-disable-line
  }

  /**
      @private
  **/
  modify () {
    const shortName = this._shortNameField.value().trim();
    const name = this._nameField.value().trim();

    if (shortName === "") {
      alert(_("Short Name is missing"));
      return;
    }

    if (name === "") {
      alert(_("Name is missing"));
      return;
    }

    this._server.setShortName(shortName);
    this._server.setName(name);
    this._serversPg.modify(this._server);


  }

}
