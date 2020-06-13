// Copyright 01-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import {_} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Server from "../../../data/Server.js"; //eslint-disable-line
import Nick from "../../../data/Nick.js"; //eslint-disable-line
import Servers from "./Servers.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Modify server codes page.
**/
export default class Codes {
  /**
      @param {!Domo} wg
      @param {!Servers} serversPg
      @param {!Server} server
      @param {!Array<!Nick>} nicks
  **/
  constructor (wg, serversPg, server, nicks) {
    this._wg = wg;
    this._serversPg = serversPg;
    this._server = server;
    this._nicks = nicks;

    this._codeFields = [];
    this._testSpans = [];

    this.view();
  }

  /**
      @private
      @param {number} id
      @return {string}
  **/
  getCode (id) {
    const svCode = this._server.codes.find(sc => sc.nickId === id);
    return svCode.code || "";
  }

  // View ----------------------------------------------------------------------
  view () {
    const sv = this._server;
    const nicks = this._nicks;

    const table = $("table")
      .att("align", "center")
      .style(`border-top: 1px solid rgb(110,130,150);
              border-bottom: 1px solid rgb(110,130,150);
              border-collapse: collapse;`)
    ;

    const tds = [];
    const len = nicks.length;
    let i = 0;
    while (i < len) {
      const ix = i;
      const nk = nicks[i++];
      const nextNk = i < len ? nicks[i] : nicks[0];

      const field = Ui.field(String(nextNk.id))
        .att("id", String(nk.id))
        .setStyle("width", "125px")
        .value(this.getCode(nk.id));
      this._codeFields.push(field);

      const span = $("span").add(Ui.img("unknown"));
      this._testSpans.push(span);

      tds.push($("td")
        .style("text-align:right;white-space: nowrap;")
        .add(Ui.link(() => this.test(nk.id, ix))
          .klass("link")
          .text(nk.name))
        .add($("span")
          .text(": "))
        .add(field)
        .add(span)
      );
    }

    const cols = 4;
    const rowsLen = Math.ceil(len / cols);
    const rows = [];
    if (rowsLen === 0) {
      rows.push($("tr").add($("td").text(_("Without Nicks"))));
    } else {
      const tdWgs = [];
      for (let i = 0; i < rowsLen; ++i) {
        const tdWg = [];
        for (let j = 0; j < cols; ++j) {
          tdWg.push(i * cols + j < len ? $("td") : null);
        }
        tdWgs.push(tdWg);
      }

      let ix = 0;
      for (let cix = 0; cix < cols; ++cix) {
        for (let rix = 0; rix < rowsLen; ++rix) {
          tdWgs[rix][cix] = tdWgs[rix][cix] === null
            ? $("td").setStyle("border-left", "1px solid rgb(110,130,150)")
            : cix === 0
              ? tds[ix++]
              : tds[ix++].setStyle("border-left", "1px solid rgb(110,130,150)")
          ;
        }
      }

      for (let i = 0; i < rowsLen; ++i) {
        const rowTds = [];
        for (let j = 0; j < cols; ++j) {
          rowTds.push(tdWgs[i][j]);
        }
        rows.push($("tr").adds(rowTds));
      }
    }

    table
      .add($("tr")
        .add($("td").att("colspan", cols)
          .style(`padding-top:4px;padding-bottom: 4px;
                  border-bottom: 1px solid rgb(110,130,150)`)
          .add($("button")
            .text(_("Reset"))
            .on("click", () => { this.reset() }))
          .add($("span")
            .text(" "))
          .add($("button")
            .text(_("Modify"))
            .on("click", () => { this.modify() }))))
      .adds(rows)
    ;

    this._wg
      .removeAll()
      .style("text-align:center")
      .add($("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add(table)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  reset () {
    //eslint-disable-next-line
    new Codes(this._wg, this._serversPg, this._server, this._nicks);
  }

  /**
      @private
  **/
  modify () {
    this._codeFields.forEach(c => {
      for (const sc of this._server.codes) {
        if (sc.nickId === Number(c.att("id"))) {

          sc.setCode(c.value().trim());
          break;
        }
      }
    });

    this._serversPg.modify(this._server);
  }

  /**
    @private
    @param {number} nickId
    @param {number} index
    @return !Promise<void>
  **/
  async test (nickId, index) {
    const rq = {
      "module": "settings",
      "source": "servers/code",
      "rq": "historicTest",
      "serverId": this._server.id,
      "nickId": nickId
    };
    this._testSpans[index]
      .removeAll()
      .add(Ui.img("wait.gif"));
    const rp = await Cts.client.ssend(rq);
    const ok = rp["ok"];
    this._testSpans[index]
      .removeAll()
      .add(Ui.img(ok ? "well" : "error"))
    ;
  }

}
