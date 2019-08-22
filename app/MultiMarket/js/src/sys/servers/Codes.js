// Copyright 20-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../../Main.js";
import Servers from "../Servers.js"; //eslint-disable-line
import Nick from "../../data/Nick.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Server from "../../data/Server.js"; //eslint-disable-line
import {ServerCode} from "../../data/Server.js"; //eslint-disable-line

// VIEW ------------

const $ = e => Ui.$(e);

/** Codes option. */
export default class Codes {

  /**
   * @param {!Servers} servers
   * @param {!Server} server
   * @param {!Array<!Nick>} nicks
   */
  constructor (servers, server, nicks) {
    this._servers = servers;
    this._server = server;
    nicks.sort((nk1, nk2) => nk1.name > nk2.name ? 1 : -1);
    this._nicks = nicks;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._table = $("table").att("align", "center")
      .style("border-top: 1px solid rgb(110,130,150);" +
             "border-bottom: 1px solid rgb(110,130,150);" +
             "border-collapse: collapse;");
    this._codeFields = [];
    this._testSpans = [];

  }

  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @param {number} id
   * @return {string}
   */
  getCode (id) {
    const svCode = this._server.codes.find(sc => sc.nickId === id);
    return svCode.code || "";
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    const sv = this._server;
    const r = $("div").style("text-align:center")
      .add($("div").klass("head").style("padding-bottom: 10px").text(sv.name))
      .add(this._table);
    this.update();
    return r;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  update () {
    const nicks = this._nicks;

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

      tds.push($("td").style("text-align:right;white-space: nowrap;")
        .add(Ui.link(() => this.test(nk.id, ix)).klass("link").text(nk.name))
        .add($("span").text(": "))
        .add(field)
        .add(span)
      );
    }

    this._table.removeAll();

    const COLS = 4;
    const rowsLen = Math.ceil(len / COLS);
    if (rowsLen === 0) {
      this._table.add($("tr").add($("td").text(_("Without Nicks"))));
      return;
    }
    const tdWgs = [];
    for (let i = 0; i < rowsLen; ++i) {
      const tdWg = [];
      for (let j = 0; j < COLS; ++j) {
        tdWg.push(i * COLS + j < len ? $("td") : null);
      }
      tdWgs.push(tdWg);
    }

    let ix = 0;
    for (let cix = 0; cix < COLS; ++cix) {
      for (let rix = 0; rix < rowsLen; ++rix) {
        tdWgs[rix][cix] = tdWgs[rix][cix] === null
          ? $("td").setStyle("border-left", "1px solid rgb(110,130,150)")
          : cix === 0
            ? tds[ix++]
            : tds[ix++].setStyle("border-left", "1px solid rgb(110,130,150)")
        ;
      }
    }

    const rows = [];
    for (let i = 0; i < rowsLen; ++i) {
      const rowTds = [];
      for (let j = 0; j < COLS; ++j) {
        rowTds.push(tdWgs[i][j]);
      }
      rows.push($("tr").adds(rowTds));
    }

    this._table
      .add($("tr")
        .add($("td").att("colspan", COLS)
          .style("padding-top:4px;padding-bottom: 4px;" +
                 "border-bottom: 1px solid rgb(110,130,150)")
          .add($("button").text(_("Reset"))
            .on("click", this.reset.bind(this)))
          .add($("span").text(" "))
          .add($("button").text(_("Modify"))
            .on("click", this.change.bind(this)))))
      .adds(rows)
    ;
  }

  /**
   * @private
   * @return {void}
   */
  reset () {
    const nicks = this._nicks;
    for (let i = 0; i < nicks.length; ++i) {
      this._codeFields[i].value(this.getCode(nicks[i].id));
      this._testSpans[i].removeAll().add(Ui.img("unknown"));
    }
  }

  /**
   * @private
   * @return {Promise}
   */
  async change () {
    const codes = this._codeFields.map(td => {
      const v = td.value().trim();
      return new ServerCode(Number(td.att("id")), v === "" ? null : v).toJs();
    });

    const rq = {
      "module": "sys",
      "source": "servers/Codes",
      "rq": "setCodes",
      "id": this._server.id,
      "codes": codes
    };
    await Main.client.send(rq);
    this._servers.update();
  }

  /**
   * @private
   * @param {number} nickId
   * @param {number} spanIx
   * @return {Promise}
   */
  async test (nickId, spanIx) {
    const rq = {
      "module": "sys",
      "source": "servers/Download", // Reuse callback
      "rq": "historicTest",
      "serverId": this._server.id,
      "nickId": nickId
    };
    this._testSpans[spanIx].removeAll().add(Ui.img("wait.gif"));
    const rp = await Main.client.sendLongRun(rq);
    const ok = rp["ok"];
    this._testSpans[spanIx].removeAll().add(Ui.img(
      ok ? "well" : "error"
    ));
  }
}
