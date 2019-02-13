// Copyright 09-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Wrule from "./wgs/Wrule.js";

const $ = Ui.$;

const mkTh = lb =>
  $("td").style("text-align: center; font-weight: bold").html(lb)
;

const cut6 = (nm, sel) =>
  "<font color='#" +
  (sel ? "000000" : "c9c9c9") +
  "'>" +
  (sel
    ? nm.length > 10 ? nm.substring(0, 9) + "." : nm
    : nm.substring(0, 3)
  ) +
  "</font>"
;

const cut60 = tx => tx.length > 60 ? tx.substring(0, 57) + "..." : tx;

/** Daily page. */
export default class Daily {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /** @private */
    this._servers = {};

    /** @private */
    this._companies = {};

    /** @private */
    this._serverDiv = $("div");
  }

  /** @private */
  serverIdControl (id) {
    const svs = this._servers;
    const svKs = Object.keys(svs);

    if (id === "") {
      return _("Name is missing");
    }
    let dup = false;
    svKs.forEach(k => { if (k === id) dup = true; });
    if (dup) {
      return _("Name is duplicated");
    }
    return "";
  }

  /** @private */
  async newServer (id, url) {
    if (url === "") {
      alert(_("Url is missing"));
      return;
    }
    const control = this.serverIdControl(id);
    if (control !== "") {
      alert(control);
      return;
    }
    const rq = {
      "source": "daily",
      "rq": "newServer",
      "id": id,
      "url": url
    };
    await this._main.client.send(rq);
    this.show();
  }

  /** @private */
  async selServer (id, currentSel) {
    const rq = {
      "source": "daily",
      "rq": "selServer",
      "id": id,
      "value": !currentSel
    };
    await this._main.client.send(rq);
    this.show();
  }

  /** @private */
  async upServer (id) {
    const rq = {
      "source": "daily",
      "rq": "upServer",
      "id": id
    };
    await this._main.client.send(rq);
    this.show();
  }

  /** @private */
  async downServer (id) {
    const rq = {
      "source": "daily",
      "rq": "downServer",
      "id": id
    };
    await this._main.client.send(rq);
    this.show();
  }

  /** @private */
  modServer1 (id) {
    this._serverDiv.removeAll().add(this.modifyDiv(id));
  }

  /** @private */
  modServer2 () {
    this._serverDiv.removeAll().add(this.newDiv());
  }

  /** @private */
  async modServer3 (oldId, newId, url) {
    let e = "";
    if (url === "") {
      e = _("Url is missing");
    }

    if (oldId !== newId) {
      e = this.serverIdControl(newId);
    }

    if (e !== "") {
      alert(e);
      return;
    }

    const rq = {
      "source": "daily",
      "rq": "modServer",
      "oldId": oldId,
      "newId": newId,
      "url": url
    };
    await this._main.client.send(rq);
    this.show();

  }

  /** @private */
  async delServer (id) {
    if (!confirm(_args(_("Delete %0?"), id))) {
      return;
    }
    const rq = {
      "source": "daily",
      "rq": "delServer",
      "id": id
    };
    await this._main.client.send(rq);
    this.show();
  }

  async changeCode (sv, nick, value) {
    const rq = {
      "source": "daily",
      "rq": "setCode",
      "server": sv,
      "nick": nick,
      "code": value
    };
    await this._main.client.send(rq);
  }

  /** @private */
  newDiv () {
    const svs = this._servers;
    const svKs = Object.keys(svs);

    const fName = Ui.field("fUrl").att("id", "fName").style("width:100px");
    const fUrl = Ui.field("btNew").att("id", "fUrl").style("width:400px");

    return $("table").att("align", "center").klass("frame3")
      .add($("tr")
        .add($("td").att("colspan", 5)
          .add($("button").att("id", "btNew").html(_("New")).on("click", () => {
            this.newServer(fName.value().trim(), fUrl.value().trim());
          })))
        .add($("td")
          .add(fName))
        .add($("td")
          .add(fUrl)))
      .add($("tr")
        .add($("td").att("colspan", 5))
        .add(mkTh(_("Name")).setStyle("text-align", "left"))
        .add(mkTh(_("URL")).setStyle("text-align", "left")))
      .adds(svKs.map((k, i) => $("tr")
        .add($("td").add(i === 0 ? Ui.img("blank")
          : Ui.link(() => { this.downServer(k) }).add(Ui.img("go-up"))))
        .add($("td").add(i === svKs.length - 1 ? Ui.img("blank")
          : Ui.link(() => { this.upServer(k) }).add(Ui.img("go-down"))))
        .add($("td")
          .add(Ui.link(() => { this.selServer(k, svs[k][0]) })
            .add(Ui.img(svs[k][0] ? "minus" : "plus"))))
        .add($("td")
          .add(Ui.link(() => { this.modServer1(k) })
            .add(Ui.img("edit"))))
        .add($("td")
          .add(Ui.link(() => { this.delServer(k) })
            .add(Ui.img("delete"))))
        .add($("td").klass("frame").html(k))
        .add($("td").klass("frame").html(cut60(svs[k][1])))
      ))
    ;
  }

  /** @private */
  modifyDiv (id) {
    const svs = this._servers;
    const svKs = Object.keys(svs);

    const fName = Ui.field("fUrl").att("id", "fName").style("width:100px");
    const fUrl = Ui.field("btNew").att("id", "fUrl").style("width:400px");

    return $("table").att("align", "center").klass("frame3")
      .add($("tr")
        .add($("td").att("colspan", 5)
          .add($("button").html(_("New")).disabled(true)))
        .add($("td")
          .add(Ui.field("").style("width:100px").disabled(true)))
        .add($("td")
          .add(Ui.field("").style("width:400px").disabled(true))))
      .add($("tr")
        .add($("td").att("colspan", 5))
        .add(mkTh(_("Name")).setStyle("text-align", "left"))
        .add(mkTh(_("URL")).setStyle("text-align", "left")))
      .adds(svKs.map(k => k === id
        ? $("tr")
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td")
            .add(Ui.link(() => { this.modServer2() })
              .add(Ui.img("cancel"))))
          .add($("td")
            .add(Ui.link(() => {
              this.modServer3(k, fName.value().trim(), fUrl.value().trim());
            }).add(Ui.img("edit"))))
          .add($("td").add(fName.value(k)))
          .add($("td").add(fUrl.value(svs[k][1])))
        : $("tr")
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td").add(Ui.img("blank")))
          .add($("td").klass("frame").style("color: #c9c9c9").html(k))
          .add($("td").klass("frame").style("color: #c9c9c9")
            .html(cut60(svs[k][1])))
      ))
    ;
  }

  /** @private */
  cosDiv () {
    const svs = this._servers;
    const svKs = Object.keys(svs);
    const cos = this._companies;
    const coKs = Object.keys(cos).sort();

    const field = (row, sv, nick, value) => {
      const nextId = row === coKs.length - 1 ? 0 : row + 1;
      const r = Ui.field(sv + "-" + String(nextId))
        .att("id", sv + "-" + String(row))
        .style("width:100px")
        .value(value);

      r.on("change", () => { this.changeCode(sv, nick, r.value().trim()) });
      return r;
    };

    const mkTable = ks => $("table").att("align", "center").klass("frame3")
      .add($("tr")
        .add(mkTh(_("Nick")))
        .adds(svKs.map(k => mkTh(cut6(k, svs[k][0])))))
      .add($("tr")
        .add($("td").att("colspan", svKs.length + 1)
          .add($("hr"))))
      .adds(ks.map((k, row) => $("tr")
        .add($("td").html(k))
        .adds([...It.range(svKs.length)].map(i => $("td")
          .add(svs[svKs[i]][0]
            ? field(row, svKs[i], k, cos[k][1][i])
            : $("div").klass("frame").style("color:#c9c9c9")
              .html(cos[k][1][i].substring(0, 3))
          )))))
    ;

    return $("div")
      .add($("h2").klass("title").html(_("Selected")))
      .add(mkTable(coKs.filter(k => cos[k][0])))
      .add($("h2").klass("title").html(_("No Selected")))
      .add(mkTable(coKs.filter(k => !cos[k][0])))
    ;
  }

  /**
   * @return {Promise}
   */
  async show () {
    const rq = {
      "source": "daily",
      "rq": "idata"
    };
    const rp = await this._main.client.send(rq);
    this._servers = rp["servers"];
    this._companies = rp["cos"];

    this._main.dom.show(
      Main.dailyPageId,
      $("div")
        .add(Wrule.mkBig(_("Daily Data Servers")))
        .add(this._serverDiv.removeAll().add(this.newDiv()))
        .add(Wrule.mkBig(_("Companies")))
        .add(this.cosDiv())
        .add(Ui.upTop("up"))
    );

  }
}

