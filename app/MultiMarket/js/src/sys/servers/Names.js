// Copyright 20-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../../Main.js";
import Servers from "../Servers.js"; //eslint-disable-line
import {_, _args} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Server from "../../data/Server.js"; //eslint-disable-line

// VIEW ------------

const $ = Ui.$;

/** Names option. */
export default class Names {

  /**
   * @param {!Servers} servers
   * @param {!Server} server
   */
  constructor (servers, server) {
    this._servers = servers;
    this._server = server;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._shortNameField = Ui.field("nameField")
      .value(this._server.shortName);
    this._nameField = Ui.field("btAccept").att("id", "nameField")
      .value(this._server.name);
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    const sv = this._server;
    return $("div").style("text-align:center")
      .add($("div").klass("head").style("padding-bottom: 10px").text(sv.name))
      .add($("table").att("align", "center")
        .style("border-top: 1px solid rgb(110,130,150);" +
               "border-bottom: 1px solid rgb(110,130,150)")
        .add($("tr")
          .add($("td").style("text-align:right").text(_("Short Name") + ":"))
          .add($("td").add(this._shortNameField)))
        .add($("tr")
          .add($("td").style("text-align:right").text(_("Name") + ":"))
          .add($("td").add(this._nameField)))
        .add($("tr")
          .add($("td").att("colspan", 2).style("padding-top:4px")
            .add($("button").text(_("Reset"))
              .on("click", this.reset.bind(this)))
            .add($("span").text(" "))
            .add($("button").text(_("Modify"))
              .on("click", this.change.bind(this))))))
    ;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {void}
   */
  reset () {
    this._shortNameField.value(this._server.shortName);
    this._nameField.value(this._server.name);
  }

  /**
   * @private
   * @return {Promise}
   */
  async change () {
    const id = this._server.id;
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

    const rq = {
      "module": "sys",
      "source": "servers/Names",
      "rq": "changeNames",
      "id": id,
      "shortName": shortName,
      "name": name
    };
    const rp = await Main.client.send(rq);

    if (rp) {
      this._servers.update();
    } else {
      alert(_args(_("Short name '%0' is duplicated"), shortName));
    }
  }


}
