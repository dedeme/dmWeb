// Copyright 17-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//import Main from "../../Main.js";
import {SERVER} from "../../consts.js";
import Servers from "../Servers.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Server from "../../data/Server.js"; //eslint-disable-line

// VIEW ------------

const $ = e => Ui.$(e);
const img = (id, title) => Ui.img(id).att("title", title);
const emptyBt = (title) => $("div")
  .style("padding:5px;" +
         "border: 1px solid #002040;border-radius: 6px;" +
         "background: #d0ddde;")
  .att("title", title)
;

/** Wserver widget. */
export default class Wserver {

  /**
   * @param {!Servers} servers
   * @param {!Server} server
   */
  constructor (servers, server) {
    this._servers = servers;
    this._server = server;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    const svs = this._servers;
    const server = this._server;

    const del = Ui.link(() => { svs.del(server) })
      .add(img("delete", _("Delete")))
    ;

    const dailyInfo = server.dailyConf === null
      ? emptyBt(_("Daily configuration deactivated"))
      : server.dailyConf.sel === SERVER.SELECTED
        ? img("star", _("Daily configuration selected"))
        : server.dailyConf.sel === SERVER.ACTIVE
          ? img("flag1", _("Daily configuration activated"))
          : img("stopped", _("Daily configuration stopped"))
    ;

    const historicInfo = server.historicConf === null
      ? emptyBt(_("Historic configuration deactivated"))
      : server.historicConf.sel === SERVER.SELECTED
        ? img("star", _("Historic configuration selected"))
        : server.historicConf.sel === SERVER.ACTIVE
          ? img("flag1", _("Historic configuration activated"))
          : img("stopped", _("Historic configuration stopped"))
    ;

    return $("table")
      .add($("tr")
        .add($("td").add(del))
        .add($("td").add(dailyInfo))
        .add($("td").add(historicInfo))
        .add($("td").add(svs.serverSelId === server.id
          ? $("span").html("<b>" + server.shortName + "</b>")
          : Ui.link(() => {
            svs.edit(server);
          }).klass("link").text(server.shortName))))
    ;
  }

}
