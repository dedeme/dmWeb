// Copyright 01-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Modify server configuration page.
**/

import ModalBox from "../../../dmjs/ModalBox.js";
import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import ListSorter from "../../../dmjs/ListSorter.js";
import {_, _args} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Server from "../../../data/Server.js"; //eslint-disable-line
import {Sconf} from "../../../data/Server.js"; //eslint-disable-line
import Servers from "./Servers.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    @param {string} id
    @param {string} nextId
    @return {!Domo}
**/
function fieldC (id, nextId) {
  return Ui.field(nextId).att("id", id).style("width:240px");
}

/**
    @param {boolean} isHistoric
    @param {string} id
    @return {string}
**/
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
    @param {string} label
    @param {string} mark
    @return {boolean} 'true' if error
**/
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

/**
    Modify server names page.
**/
export default class Configuration {
  /**
      @param {!Domo} wg
      @param {!Servers} serversPg
      @param {boolean} isHistoric
      @param {!Server} server
  **/
  constructor (wg, serversPg, isHistoric, server) {
    this._wg = wg;
    this._serversPg = serversPg;
    this._isHistoric = isHistoric;
    this._server = server;

    this._isActivated = isHistoric
      ? server.historicConf !== null
      : server.dailyConf !== null
    ;
    this._fieldsList = [];

    this._table = $("table")
      .att("align", "center")
      .style(`border-top: 1px solid rgb(110,130,150);
              border-bottom: 1px solid rgb(110,130,150);
              border-collapse: collapse;`);

    this._activateBt = $("button").on("click", () => { this.activate() });
    this._resetBt = $("button")
      .text(_("Reset"))
      .on("click", () => { this.reset() });
    this._modifyBt = $("button")
      .text(_("Modify"))
      .on("click", () => { this.modify() });

    this._sel = $("input").att("type", "radio").att("name", "sel");
    this._active = $("input").att("type", "radio").att("name", "sel");
    this._stopped = $("input").att("type", "radio").att("name", "sel");

    this._testDiv = $("div");

    this._cmdWget = $("input").att("type", "radio").att("name", "cmd");
    this._cmdPuppeteer = $("input").att("type", "radio").att("name", "cmd");
    this._url = Ui.field("dateSep").att("id", "url").style("width:600px");
    this._regex = $("textarea").att("rows", "3").att("cols", 80);

    this._isoDate = $("input").att("type", "checkbox");
    this._dateSep = Ui.field("tableStart")
      .att("id", "dateSep")
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

    this._msgWait = $("div");

    this.view();
  }

  // View ----------------------------------------------------------------------
  view () {
    const sv = this._server;

    this._table
      .add($("tr")
        .add($("td")
          .style("width: 100px"))
        .add($("td")
          .style("width: 250px"))
        .add($("td")
          .style("width: 100px"))
        .add($("td")
          .style("width: 250px")))
      .add($("tr")
        .add($("td")
          .style(`padding-top:4px;padding-bottom: 4px;
                  border-bottom: 1px solid rgb(110,130,150);
                  text-align:left`)
          .add(this._activateBt))
        .add($("td")
          .style(`padding-top:4px;padding-bottom: 4px;
                  border-bottom: 1px solid rgb(110,130,150)`))
        .add($("td")
          .att("colspan", 2)
          .style(`padding-top:4px;padding-bottom: 4px;
                  border-bottom: 1px solid rgb(110,130,150);
                  text-align:right`)
          .add(this._resetBt)
          .add($("span")
            .text(" "))
          .add(this._modifyBt)))
    ;

    if (this._isActivated) {
      this._activateBt.text(_("Remove"));

      this._table
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style("text-align:left")
            .add(this._stopped)
            .add($("span")
              .html("&nbsp;"))
            .add($("span")
              .html(_("Stopped")))
            .add($("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(this._active)
            .add($("span")
              .html("&nbsp;"))
            .add($("span")
              .html(_("Active")))
            .add($("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(this._sel)
            .add($("span")
              .html("&nbsp;"))
            .add($("span")
              .html(_("Selected"))))
          .add($("td")
            .style("text-align:right")
            .add(Ui.link(() => { this.test() })
              .klass("link")
              .text(_("Test") + ": ")))
          .add($("td")
            .style("text-align:left")
            .add(this._testDiv)))
        .add($("tr")
          .add($("td")
            .att("colspan", 4)
            .style("text-align:left")
            .add(this._cmdWget)
            .add($("span")
              .html("&nbsp;"))
            .add($("span")
              .html(Cts.wget))
            .add($("span")
              .html("&nbsp;&nbsp;&nbsp;"))
            .add(this._cmdPuppeteer)
            .add($("span")
              .html("&nbsp;"))
            .add($("span")
              .html(Cts.puppeteer))))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("URL") + ": "))
          .add($("td")
            .att("colspan", 3)
            .style("text-align:left")
            .add(this._url)))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("RegEx") + ": "))
          .add($("td").att("colspan", 3)
            .style("text-align:left")
            .add(this._regex)))
      ;

      if (this._isHistoric) {
        this._table
          .add($("tr")
            .add($("td")
              .style("text-align:right")
              .add(this._isoDate))
            .add($("td")
              .style("text-align:left")
              .text(_("Is ISO date?")))
            .add($("td")
              .style("text-align:right")
              .add(this._dateSep))
            .add($("td")
              .style("text-align:left")
              .text(_("Date separator"))))
        ;
      }

      this._table
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .add(this._isoNum))
          .add($("td")
            .style("text-align:left")
            .text(_("Is ISO number?")))
          .add($("td"))
          .add($("td")))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("Fields order") + ": "))
          .add($("td")
            .style("text-align:left")
            .add(this._fieldsDiv))
          .add($("td"))
          .add($("td")))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("Table start") + ": "))
          .add($("td")
            .style("text-align:left")
            .add(this._tableStart))
          .add($("td")
            .style("text-align:right")
            .text(_("Table end") + ": "))
          .add($("td")
            .style("text-align:left")
            .add(this._tableEnd)))
        .add($("tr")
          .add($("td")
            .style("text-align:right")
            .text(_("Row start") + ": "))
          .add($("td")
            .style("text-align:left")
            .add(this._rowStart))
          .add($("td")
            .style("text-align:right")
            .text(_("Row end") + ": "))
          .add($("td")
            .style("text-align:left")
            .add(this._rowEnd)))
        .adds(this._cells.map(c =>
          $("tr")
            .add($("td").style("text-align:right").add(c[0]))
            .add($("td").style("text-align:left").add(c[1]))
            .add($("td").style("text-align:right").add(c[2]))
            .add($("td").style("text-align:left").add(c[3]))
        ))
      ;

      const server = this._server;
      const isHistoric = this._isHistoric;
      const conf = isHistoric ? server.historicConf : server.dailyConf;

      if (conf.sel === Cts.serverStopped) {
        this._stopped.checked(true);
      } else if (conf.sel === Cts.serverActive) {
        this._active.checked(true);
      } else {
        this._sel.checked(true);
      }
      this._testDiv.removeAll().add(Ui.img("unknown"));

      if (conf.cmd === Cts.wget) this._cmdWget.checked(true);
      else this._cmdPuppeteer.checked(true);
      this._url.value(conf.url);
      this._regex.value(conf.regex);

      this._isoDate.checked(conf.isIsoDate);
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

    } else {
      this._activateBt.text(_("Activate"));
      this._resetBt.disabled(true);
      this._modifyBt.disabled(true);
    }

    this._wg
      .removeAll()
      .style("text-align:center")
      .add(this._msgWait)
      .add($("div")
        .klass("head")
        .style("padding-bottom: 10px")
        .text(sv.name))
      .add(this._table)
    ;
  }

  /** @private */
  setWait (nickName) {
    this._msgWait.removeAll();

    if (nickName !== "") {
      const box = new ModalBox(
        $("div")
          .add($("div")
            .style("text-align:center")
            .add(Ui.img("wait2.gif").klass("frame")))
          .add($("div").style("text-align:center").html(nickName)),
        false
      );
      this._msgWait.add(box.wg);
      box.show(true);
    }
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return void
  **/
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

    this._fieldsDiv
      .removeAll()
      .add($("table")
        .klass("white")
        .add($("tr")
          .adds(ls.ups.map(e => $("td").add(e))))
        .add($("tr")
          .adds(ls.downs.map(e => $("td").add(e))))
        .add($("tr")
          .adds(this._fieldsList.map(e => $("td")
            .style("border-top: 1px solid rgb(110,130,150)")
            .html(e)))))
    ;

    this._fieldsList.forEach((e, i) => {
      const n = fieldToLabel(this._isHistoric, e);
      this._cells[i][0].removeAll().html(n + " " + _("start") + ": ");
      this._cells[i][2].removeAll().html(n + " " + _("end") + ": ");
    });
  }

  /**
      @private
  **/
  activate () {
    const sv = this._server;

    if (this._isActivated) {
      if (!confirm(_args(
        _("Remove %0 configuration?"),
        this._isHistoric ? _("historic") : _("daily")
      ))) {
        return;
      }
      if (this._isHistoric) {
        sv.setHistoricConf(null);
      } else {
        sv.setDailyConf(null);
      }
    } else if (this._isHistoric) {
      sv.setHistoricConf(new Sconf(
        Cts.wget, "", "", Cts.serverActive,
        true, "/", true,
        "DOCXNV", // Date, Open, Close, maX, miN, Volume
        "", "",
        "", "",
        ["", "", "", "", "", ""],
        ["", "", "", "", "", ""]
      ));
    } else {
      sv.setDailyConf(new Sconf(
        Cts.wget, "", "", Cts.serverActive,
        true, "/", true,
        "CQ", // Code, Quote
        "", "",
        "", "",
        ["", ""],
        ["", ""]
      ));
    }

    this._serversPg.modify(this._server);
  }

  /**
      @private
  **/
  reset () {
    //eslint-disable-next-line
    new Configuration(
      this._wg, this._serversPg, this._isHistoric, this._server
    );
  }

  /**
      @private
  **/
  modify () {
    const cmd = this._cmdWget.checked() ? Cts.wget : Cts.puppeteer;
    const url = this._url.value().trim();
    if (url === "") {
      alert(_("URL is missing"));
      return;
    }
    const regex = this._regex.value().trim();
    const sel = this._stopped.checked()
      ? Cts.serverStopped
      : this._active.checked() ? Cts.serverActive : Cts.serverSelected
    ;
    const isIsoDate = this._isoDate.checked();
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

    const cf = this._isHistoric
      ? this._server.historicConf
      : this._server.dailyConf
    ;

    cf.setCmd(cmd);
    cf.setUrl(url);
    cf.setRegex(regex);
    cf.setSel(sel);
    cf.setIsIsoDate(isIsoDate);
    cf.setDateSeparator(dateSeparator);
    cf.setIsIsoNumber(isIsoNumber);
    cf.setFieldsType(fieldsType);
    cf.setTableStart(tableStart);
    cf.setTableEnd(tableEnd);
    cf.setRowStart(rowStart);
    cf.setRowEnd(rowEnd);
    cf.setCellsStart(cellsStart);
    cf.setCellsEnd(cellsEnd);

    this._serversPg.modify(this._server);
  }

  /**
      @private
  **/
  async test () {
    if (this._isHistoric) {
      let ok = true;
      for (const nkCode of this._server.codes) {
        this.setWait(nkCode.code);

        const rp = await Cts.client.ssend({
          "module": "settings",
          "source": "servers/configuration",
          "rq": "historicTest",
          "serverId": this._server.id,
          "nickId": nkCode.nickId
        });

        if (!rp["ok"]) {
          ok = false;
        }
      }
      this.setWait("");
      this._testDiv.removeAll().add(Ui.img(ok ? "well" : "error"));
    } else {
      this._testDiv.removeAll().add(Ui.img("wait.gif"));
      const rp = await Cts.client.ssend({
        "module": "settings",
        "source": "servers/configuration",
        "rq": "dailyTest",
        "serverId": this._server.id
      });
      const ok = rp["ok"];
      this._testDiv.removeAll().add(Ui.img(ok ? "well" : "error"));
    }
  }

}
