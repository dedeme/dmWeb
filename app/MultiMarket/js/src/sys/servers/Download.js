// Copyright 22-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Daily and historic download options. */

import Main from "../../Main.js";
import {SERVER} from "../../consts.js";
import Servers from "../Servers.js"; //eslint-disable-line
import {_, _args} from "../../I18n.js";
import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import ListSorter from "../../dmjs/ListSorter.js";
import Server from "../../data/Server.js"; //eslint-disable-line
import {Rconf} from "../../data/Server.js"; //eslint-disable-line

const $ = Ui.$;

/**
 * @param {string} id
 * @param {string} nextId
 * @return {!Domo}
 */
function fieldC (id, nextId) {
  return Ui.field(nextId).att("id", id).style("width:240px");
}

/**
 * @param {boolean} isHistoric
 * @param {string} id
 * @return {string}
 */
function fieldToLabel (isHistoric, id) {
  return id === "C" && isHistoric ? _("CloseN")
    : id === "C" ? _("Code")
      : id === "Q" ? _("CloseN")
        : id === "D" ? _("Date")
          : id === "O" ? _("Open")
            : id === "X" ? _("Max")
              : id === "N" ? _("Min")
                : _("Volume")
  ;
}

/**
 * @param {string} label
 * @param {string} mark
 * @return {boolean} 'true' if error
 */
function markError (label, mark) {
  if (mark === "") {
    alert(_args(_("%0 is missing"), label));
    return true;
  }
  const marks = mark.split("|");
  for (let i = 0; i < marks.length; ++i) {
    const m = marks[i].trim();
    if (m === "") {
      alert(_args(_("Values of '%0' have blanks"), label));
      return true;
    }
  }
  return false;
}

/** Daily and historic download options. */
export default class Downloads {

  /**
   * @param {boolean} isHistoric
   * @param {!Servers} servers
   * @param {!Server} server
   */
  constructor (isHistoric, servers, server) {
    this._isHistoric = isHistoric;
    this._servers = servers;
    this._server = server;

    this._isActivated = isHistoric
      ? server.historicConf !== null
      : server.dailyConf !== null
    ;

    this._fieldsList = [];

    // VIEW --------
    // TTTTTTTTTTTTT

    this._table = $("table").att("align", "center")
      .style("border-top: 1px solid rgb(110,130,150);" +
             "border-bottom: 1px solid rgb(110,130,150);" +
             "border-collapse: collapse;");

    this._activateBt = $("button").on("click", this.activate.bind(this));
    this._resetBt = $("button").text(_("Reset"))
      .on("click", this.reset.bind(this));
    this._modifyBt = $("button").text(_("Modify"))
      .on("click", this.modify.bind(this));

    this._sel = $("input").att("type", "radio").att("name", "sel");
    this._active = $("input").att("type", "radio").att("name", "sel");
    this._stopped = $("input").att("type", "radio").att("name", "sel");

    this._testDiv = $("div");

    this._url = Ui.field("dateSep").att("id", "url").style("width:600px");

    this._dateEu = $("input").att("type", "checkbox");
    this._dateSep = Ui.field("tableStart").att("id", "dateSep")
      .style("width:10px");

    this._isoNum = $("input").att("type", "checkbox");

    this._fieldsDiv = $("div");

    this._tableStart = fieldC("tableStart", "tableEnd");
    this._tableEnd = fieldC("tableEnd", "rowStart");

    this._rowStart = fieldC("rowStart", "rowEnd");
    this._rowEnd = fieldC("rowEnd", "cSt0");

    this._cells = [];
    const len = isHistoric ? 6 : 2;
    for (let i = 0; i < len; ++i) {
      this._cells.push([
        $("span"), fieldC("cSt" + String(i), "cEnd" + String(i)),
        $("span"), i === len - 1
          ? fieldC("cEnd" + String(i), "url")
          : fieldC("cEnd" + String(i), "cSt" + String(i + 1))
      ]);
    }

  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Domo} */
  get wg () {
    const sv = this._server;
    const r = $("div").style("text-align:center")
      .add($("div").klass("head").style("padding-bottom: 10px").text(sv.name))
      .add(this._table
        .add($("tr")
          .add($("td").style("width: 100px"))
          .add($("td").style("width: 250px"))
          .add($("td").style("width: 100px"))
          .add($("td").style("width: 250px")))
        .add($("tr")
          .add($("td")
            .style("padding-top:4px;padding-bottom: 4px;" +
                   "border-bottom: 1px solid rgb(110,130,150);" +
                   "text-align:left")
            .add(this._activateBt))
          .add($("td")
            .style("padding-top:4px;padding-bottom: 4px;" +
                   "border-bottom: 1px solid rgb(110,130,150)"))
          .add($("td").att("colspan", 2)
            .style("padding-top:4px;padding-bottom: 4px;" +
                   "border-bottom: 1px solid rgb(110,130,150);" +
                   "text-align:right")
            .add(this._resetBt)
            .add($("span").text(" "))
            .add(this._modifyBt)))
      );
    this.update();
    return r;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {void}
   */
  setFieldsOrder () {
    const ls = new ListSorter(
      () => Ui.img("blank"),
      () => Ui.img("go-previous"),
      () => Ui.img("go-next"),
      this._fieldsList,
      (ls) => {
        this._fieldsList = ls;
        this.setFieldsOrder();
      }
    );

    this._fieldsDiv.removeAll().add($("table").klass("white")
      .add($("tr")
        .adds(ls.ups.map(e => $("td").add(e))))
      .add($("tr")
        .adds(ls.downs.map(e => $("td").add(e))))
      .add($("tr")
        .adds(this._fieldsList.map(e => $("td")
          .style("border-top: 1px solid rgb(110,130,150)")
          .html(e))))
    );

    this._fieldsList.forEach((e, i) => {
      const n = fieldToLabel(this._isHistoric, e);
      this._cells[i][0].removeAll().html(n + " " + _("start") + ": ");
      this._cells[i][2].removeAll().html(n + " " + _("end") + ": ");
    });
  }

  /**
   * @private
   * @return {void}
   */
  reset () {

    const server = this._server;
    const isHistoric = this._isHistoric;
    const conf = isHistoric ? server.historicConf : server.dailyConf;

    if (conf.sel === SERVER.STOPPED) {
      this._stopped.checked(true);
    } else if (conf.sel === SERVER.ACTIVE) {
      this._active.checked(true);
    } else {
      this._sel.checked(true);
    }
    this._testDiv.removeAll().add(Ui.img("unknown"));

    this._url.value(conf.url);

    this._dateEu.checked(conf.isDateEu);
    this._dateSep.value(conf.dateSeparator);

    this._isoNum.checked(conf.isIsoNumber);

    this._fieldsList = [];
    for (let i = 0; i < conf.fieldsType.length; ++i) {
      this._fieldsList.push(conf.fieldsType.charAt(i));
    }
    this.setFieldsOrder();

    this._tableStart.value(conf.tableStart);
    this._tableEnd.value(conf.tableEnd);

    this._rowStart.value(conf.rowStart);
    this._rowEnd.value(conf.rowEnd);

    for (let i = 0; i < conf.cellsStart.length; ++i) {
      this._cells[i][1].value(conf.cellsStart[i]);
      this._cells[i][3].value(conf.cellsEnd[i]);
    }
  }

  /**
   * @private
   * @return {!Promise}
   */
  async modify () {
    const url = this._url.value().trim();
    if (url === "") {
      alert(_("URL is missing"));
      return;
    }
    const sel = this._stopped.checked() ? SERVER.STOPPED
      : this._active.checked() ? SERVER.ACTIVE : SERVER.SELECTED;
    const isDateEu = this._dateEu.checked();
    const dateSeparator = this._dateSep.value().trim();
    if (dateSeparator === "") {
      alert(_("Date separator is missing"));
      return;
    }
    const isIsoNumber = this._isoNum.checked();
    const fieldsType = this._fieldsList.join("");

    const tableStart = this._tableStart.value().trim();
    if (markError("Table start", tableStart)) return;
    const tableEnd = this._tableEnd.value().trim();
    if (markError("Table end", tableEnd)) return;

    const rowStart = this._rowStart.value().trim();
    if (markError("Row start", rowStart)) return;
    const rowEnd = this._rowEnd.value().trim();
    if (markError("Row end", rowEnd)) return;
    const cellsStart = [];
    const cellsEnd = [];
    for (let i = 0; i < this._cells.length; ++i) {
      const c = this._cells[i];
      let label = fieldToLabel(this._isHistoric, this._fieldsList[i]) +
        " " + _("start");
      const st = c[1].value().trim();
      if (markError(label, st)) return;
      cellsStart.push(st);

      label = fieldToLabel(this._isHistoric, this._fieldsList[i]) +
        " " + _("end");
      const end = c[3].value().trim();
      if (markError(label, end)) return;
      cellsEnd.push(end);
    }

    const rq = {
      "module": "sys",
      "source": "servers/Download",
      "rq": "modify",
      "id": this._server.id,
      "historic": this._isHistoric,
      "conf": new Rconf(
        url,
        sel,
        isDateEu,
        dateSeparator,
        isIsoNumber,
        fieldsType,
        tableStart,
        tableEnd,
        rowStart,
        rowEnd,
        cellsStart,
        cellsEnd
      ).toJs()
    };
    await Main.client.send(rq);
    this._servers.sysMain.update();
  }

  /**
   * @private
   * @return {!Promise}
   */
  async activate () {
    const conf = this._isHistoric
      ? new Rconf(
        "", SERVER.ACTIVE,
        true, "/", true,
        "DOCXNV", // Date, Open, Close, maX, miN, Volume
        "", "",
        "", "",
        ["", "", "", "", "", ""],
        ["", "", "", "", "", ""]
      )
      : new Rconf(
        "", SERVER.ACTIVE,
        true, "/", true,
        "CQ", // Code, Quote
        "", "",
        "", "",
        ["", ""],
        ["", ""]
      )
    ;
    if (this._isActivated) {
      if (!confirm(_args(
        _("Remove %0 configuration?"),
        this._isHistoric ? _("historic") : _("daily")
      ))) {
        return;
      }
    }
    const rq = {
      "module": "sys",
      "source": "servers/Download",
      "rq": "activate",
      "id": this._server.id,
      "historic": this._isHistoric,
      "conf": conf.toJs()
    };
    await Main.client.send(rq);
    this._servers.sysMain.update();
  }

  async test () {
    if (this._isHistoric) {
      let ok = true;
      const len = this._server.codes.length;
      for (let i = 0; i < len; ++i) {
        const code = this._server.codes[i].code || "Null";
        this._testDiv.removeAll().text(code);
        const rq = {
          "module": "sys",
          "source": "servers/Download",
          "rq": "historicTest",
          "serverId": this._server.id,
          "nickId": this._server.codes[i].nickId
        };
        const rp = await Main.client.sendLongRun(rq);
        ok = ok && (rp["longRunEnd"] ? rp["ok"] : false);
      }
      this._testDiv.removeAll().add(Ui.img(ok ? "well" : "error"));
    } else {
      this._testDiv.removeAll().add(Ui.img("wait.gif"));
      const rq = {
        "module": "sys",
        "source": "servers/Download",
        "rq": "dailyTest",
        "serverId": this._server.id
      };
      const rp = await Main.client.sendLongRun(rq);
      const ok = rp["longRunEnd"] ? rp["ok"] : false;
      this._testDiv.removeAll().add(Ui.img(ok ? "well" : "error"));
    }
  }

  /**
   * @private
   * @return {void}
   */
  update () {
    if (this._isActivated) {
      this._activateBt.text(_("Remove"));

      this._table
        .add($("tr")
          .add($("td").att("colspan", 2).style("text-align:left")
            .add(this._stopped).add($("span").html("&nbsp;"))
            .add($("span").html(_("Stopped")))
            .add($("span").html("&nbsp;&nbsp;&nbsp;"))
            .add(this._active).add($("span").html("&nbsp;"))
            .add($("span").html(_("Active")))
            .add($("span").html("&nbsp;&nbsp;&nbsp;"))
            .add(this._sel).add($("span").html("&nbsp;"))
            .add($("span").html(_("Selected"))))
          .add($("td").style("text-align:right")
            .add(Ui.link(this.test.bind(this)).klass("link")
              .text(_("Test") + ": ")))
          .add($("td").style("text-align:left").add(this._testDiv)))
        .add($("tr")
          .add($("td").style("text-align:right").text(_("URL") + ": "))
          .add($("td").att("colspan", 3).style("text-align:left")
            .add(this._url)))
      ;
      if (this._isHistoric) {
        this._table
          .add($("tr")
            .add($("td").style("text-align:right").add(this._dateEu))
            .add($("td").style("text-align:left").text(_("Is ISO date?")))
            .add($("td").style("text-align:right").add(this._dateSep))
            .add($("td").style("text-align:left").text(_("Date separator"))))
        ;
      }
      this._table
        .add($("tr")
          .add($("td").style("text-align:right").add(this._isoNum))
          .add($("td").style("text-align:left").text(_("Is ISO number?")))
          .add($("td"))
          .add($("td")))
        .add($("tr")
          .add($("td").style("text-align:right").text(_("Fields order") + ": "))
          .add($("td").style("text-align:left").add(this._fieldsDiv))
          .add($("td"))
          .add($("td")))
        .add($("tr")
          .add($("td").style("text-align:right").text(_("Table start") + ": "))
          .add($("td").style("text-align:left").add(this._tableStart))
          .add($("td").style("text-align:right").text(_("Table end") + ": "))
          .add($("td").style("text-align:left").add(this._tableEnd)))
        .add($("tr")
          .add($("td").style("text-align:right").text(_("Row start") + ": "))
          .add($("td").style("text-align:left").add(this._rowStart))
          .add($("td").style("text-align:right").text(_("Row end") + ": "))
          .add($("td").style("text-align:left").add(this._rowEnd)))
        .adds(this._cells.map(c =>
          $("tr")
            .add($("td").style("text-align:right").add(c[0]))
            .add($("td").style("text-align:left").add(c[1]))
            .add($("td").style("text-align:right").add(c[2]))
            .add($("td").style("text-align:left").add(c[3]))
        ))
      ;
      this.reset();
    } else {
      this._activateBt.text(_("Activate"));
      this._resetBt.disabled(true);
      this._modifyBt.disabled(true);
    }
  }
}
