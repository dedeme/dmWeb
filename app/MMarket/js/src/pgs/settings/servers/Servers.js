// Copyright 28-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import {Menu, MenuEntry} from "../../../dmjs/Menu.js";
import Maybe from "../../../dmjs/Maybe.js";
import {_, _args} from "../../../I18n.js";
import Msg from "../../../wgs/Msg.js";
import Cts from "../../../data/Cts.js";
import Server from "../../../data/Server.js";
import Nick from "../../../data/Nick.js";
import Wserver from "./wgs/Wserver.js";
import Names from "./Names.js";
import Configuration from "./Configuration.js";
import Codes from "./Codes.js";

const $ = e => Ui.$(e);

/**
    Servers page.
**/
export default class Servers {

  /**
      @param {!Domo} wg
      @param {!Array<!Server>} servers
      @param {!Array<!Nick>} nicks
  **/
  constructor (wg, servers, nicks) {
    this._wg = wg;
    this._servers = servers;
    this._nicks = nicks;
    this._withVolume = true;
    this._selectedServer = -1;
    this._selectedTab = "names";

    this._newServer = Ui.field("newBt").style("width:100px");
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
    @private
    @return void
  **/
  view () {
    const serverListWg = $("div");
    const body = $("div");
    const servers = this._servers;
    const sel = this._selectedServer;

    if (servers.length === 0) {
      serverListWg
        .removeAll()
        .add($("div")
          .klass("frame")
          .style("text-align:center")
          .html(_("Without<br>Servers")))
      ;

      body
        .removeAll()
        .add($("table")
          .klass("frame")
          .att("align", "center")
          .add($("tr")
            .add($("td")
              .text(_("Without Data")))))
      ;
    } else {
      serverListWg
        .removeAll()
        .add($("table")
          .klass("submenu")
          .adds(servers.map((sv, i) => {
            const td = $("td");
            // eslint-disable-next-line
            new Wserver(
              td,
              this,
              sv,
              i === this._selectedServer
            );
            return $("tr").add(td);
          })))
      ;

      if (sel < 0 || sel > servers.length) {
        body
          .removeAll()
          .add($("table")
            .klass("frame")
            .att("align", "center")
            .add($("tr")
              .add($("td")
                .text(_("No server selected")))))
        ;
      } else {
        const server = servers[sel];
        const body2 = $("div");

        const lopts = [
          Menu.toption("names", _("Names"), () => { this.setTab("names") }),
          Menu.separator(),
          Menu.toption("daily", _("Daily"), () => { this.setTab("daily") }),
          Menu.separator(),
          Menu.toption(
            "historic", _("Historic"), () => { this.setTab("historic") }
          ),
          Menu.separator(),
          Menu.toption("codes", _("Codes"), () => { this.setTab("codes") }),
        ];
        const ropts = [
          new MenuEntry(Maybe.nothing, $("span").text(server.name))
        ];
        const menu = new Menu(lopts, ropts, this._selectedTab);

        switch (this._selectedTab) {
        case "daily":
          new Configuration(body2, this, false, server); // eslint-disable-line
          break;
        case "historic":
          new Configuration(body2, this, true, server); // eslint-disable-line
          break;
        case "codes":
          new Codes(body2, this, server, this._nicks); // eslint-disable-line
          break;
        default:
          new Names(body2, this, server); // eslint-disable-line
        }

        body
          .removeAll()
          .add($("table")
            .klass("main")
            .add($("tr")
              .add($("td")
                .add(menu.wg)))
            .add($("tr")
              .add($("td")
                .add(body2))))
        ;
      }
    }

    this._wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("vertical-align:top;width:5px")
            .add($("table")
              .klass("home")
              .add($("tr")
                .add($("td")
                  .add(this._newServer)))
              .add($("tr")
                .add($("td")
                  .style("text-align:center")
                  .add($("button")
                    .att("id", "newBt")
                    .text(_("New"))
                    .on("click", () => this.newServer()))))
              .add($("tr")
                .add($("td")
                  .add($("hr"))))
              .add($("tr")
                .add($("td")
                  .add(serverListWg)))))
          .add($("td")
            .style("vertical-align:top")
            .add(body))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async newServer () {
    const server = this._newServer.value().trim();
    if (server === "") {
      Msg.error(_("Server name is empty"));
      return;
    }
    if (this._servers.some(e => e.shortName === server)) {
      Msg.error(_("Server name is duplicated"));
      return;
    }
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "servers",
      "rq": "new",
      "server": server
    });
    if (!rp["ok"]) {
      Msg.error(Cts.failMsg);
    }
    Servers.mk(this._wg);
  }

  /**
      @param {!Server} server
      @return void
  **/
  async del (server) {
    if (!confirm(_args(_("Delete '%0'?"), server.shortName))) {
      return;
    }
    await Cts.client.ssend({
      "module": "settings",
      "source": "servers",
      "rq": "del",
      "id": server.id
    });
    Servers.mk(this._wg);
  }

  /**
      @param {!Server} server
      @return void
  **/
  sel (server) {
    const svs = this._servers;
    const id = server.id;
    let ix = -1;
    for (let i = 0; i < svs.length; ++i) {
      if (svs[i].id === id) {
        ix = i;
        break;
      }
    }
    this._selectedServer = ix;
    this.view();
  }

  /**
      @param {string} tabId
      @return void
  **/
  setTab (tabId) {
    this._selectedTab = tabId;
    this.view();
  }

  /**
      @param {!Server} server
      @return void
  **/
  async modify (server) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "servers",
      "rq": "modify",
      "server": server.toJs()
    });
    if (rp["ok"]) {
      this.view();
      Msg.ok(Cts.okMsg);
    } else {
      Msg.error(Cts.failMsg);
      Servers.mk(this._wg);
    }
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return {!Promise<!Servers>}
  **/
  static async mk (wg) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "servers",
      "rq": "idata"
    });
    const /** !Array<!Server> **/ servers =
      rp["servers"].map(e => Server.fromJs(e))
        .sort((e1, e2) => e1.shortName > e2.shortName ? 1 : -1);
    const /** !Array<!Nick> **/ nicks =
      rp["nicks"].map(e => Nick.fromJs(e))
        .sort((e1, e2) => e1.name > e2.name ? 1 : -1);
    return new Servers(wg, servers, nicks);
  }
}
