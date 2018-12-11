// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Nick from "./data/Nick.js";
import Wrule from "./wgs/Wrule.js";
import WserverId from "./wgs/WserverId.js";

const $ = Ui.$;

/** Servers page. */
export default class Servers {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  async go (server) {
    const data = {
      "source": "servers",
      "rq": "setSelServer",
      "name": server
    };
    await this._main.client.send(data);
    this._main.run();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  nick_list (sel, nicks, codes) {
    const len = nicks.length;
    if (len === 0) {
      return $("table").att("align", "center")
        .add($("tr")
          .add($("td").klass("frame4").html(_("Without nicks"))));
    }
    const cols = 4;
    const rows = Math.floor((len - 1) / cols) + 1;
    return $("table").style("width:100%")
      .adds([...It.range(rows).map(row =>
        $("tr").adds([...It.range(cols).map(col => {
          const ix = row * cols + col;
          if (ix < len) {
            return $("td").klass("frame").style("text-align:center;width:25%;")
              .add(Wrule.mkSmall(nicks[ix].nick))
              .add(new WserverId(
                this._main, sel, nicks[ix].id, codes[ix]
              ).wg());
          }
          return $("td");
        })])
      )]);
  }

  /** @private */
  show2 (servers, sel, nicks, codes) {
    servers.sort();
    const self = this;
    const entry = (server) => Ui.link(() => {
      self.go(server);
    }).klass(server === sel ? "frame" : "link").html(server);
    const separator = () => $("span").html(" · ");
    const w = $("div")
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td")
            .add(entry(servers[0]))
            .add(separator())
            .add(entry(servers[1]))
            .add(separator())
            .add(entry(servers[2])))))
      .add(Wrule.mkBig(sel))
      .add(self.nick_list(sel, nicks, codes))

    ;
    this._main.dom.show(Main.serversPageId, w);
  }




  /**
   * @return {Promise}
   */
  async show () {
    const self = this;
    const data = {
      "source": "servers",
      "rq": "idata"
    };
    const rp = await self._main.client.send(data);
    const servers = rp["servers"];
    const selServer = rp["selServer"];
    const nicks = rp["nicks"].map(n => Nick.fromJson(n));
    nicks.sort((n1, n2) => n1.nick.localeCompare(n2.nick));
    const codes = [];

    const total = nicks.length;
    let ix = 0;
    const bar = $("div").att("id", "progressFore");
    const wg = $("div").style("width:250px")
      .add($("div").att("id", "progressBack").add(bar));
    const box = self._main.dom.addWait(wg);
    box.show(true);

    const sh = async it => {
      if (it.has) {
        const data = {
          "source": "servers",
          "rq": "nickCode",
          "serverId": selServer,
          "nickId": it.value.id
        };
        const rp = await self._main.client.send(data);
        codes.push(rp["code"]);
        ++ix;
        bar.setStyle("width", `${Math.floor(ix * 100 / total)}%`);
        sh(it.next);
      } else {
        box.show(false);
        self.show2(servers, selServer, nicks, codes);
      }
    };
    sh(It.from(nicks));
  }
}

