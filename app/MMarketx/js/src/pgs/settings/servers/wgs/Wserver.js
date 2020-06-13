// Copyright 30-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Cts from "../../../../data/Cts.js";
import Servers from "../Servers.js"; //eslint-disable-line
import {_} from "../../../../I18n.js";
import Ui from "../../../../dmjs/Ui.js";
import Domo from "../../../../dmjs/Domo.js"; //eslint-disable-line
import Server from "../../../../data/Server.js"; //eslint-disable-line

const $ = e => Ui.$(e);

const img = (id, title) => Ui.img(id).att("title", title);
const emptyBt = (title) => $("div")
  .style("padding:5px;" +
         "border: 1px solid #002040;border-radius: 6px;" +
         "background: #d0ddde;")
  .att("title", title)
;

/**
    Wserver widget.
**/
export default class Wserver {

  /**
      @param {!Domo} wg
      @param {!Servers} servers
      @param {!Server} server
      @param {boolean} isSelected
  **/
  constructor (wg, servers, server, isSelected) {
    this._wg = wg;
    this._servers = servers;
    this._server = server;
    this._isSelected = isSelected;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    const server = this._server;

    const dailyInfo = server.dailyConf === null
      ? emptyBt(_("Daily configuration deactivated"))
      : server.dailyConf.sel === Cts.serverSelected
        ? img("star", _("Daily configuration selected"))
        : server.dailyConf.sel === Cts.serverActive
          ? img("flag1", _("Daily configuration activated"))
          : img("stopped", _("Daily configuration stopped"))
    ;

    const historicInfo = server.historicConf === null
      ? emptyBt(_("Historic configuration deactivated"))
      : server.historicConf.sel === Cts.serverSelected
        ? img("star", _("Historic configuration selected"))
        : server.historicConf.sel === Cts.serverActive
          ? img("flag1", _("Historic configuration activated"))
          : img("stopped", _("Historic configuration stopped"))
    ;

    this._wg
      .removeAll()
      .add($("table")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => { this._servers.del(server) })
              .add(img("delete", _("Delete")))))
          .add($("td").add(dailyInfo))
          .add($("td").add(historicInfo))
          .add($("td").add(this._isSelected
            ? $("span")
              .html("<b>" + server.shortName + "</b>")
            : Ui.link(() => {
              this._servers.sel(server);
            }).klass("link")
              .text(server.shortName)))))
    ;
  }
}
