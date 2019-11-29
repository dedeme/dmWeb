// Copyright 15-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Nicks from "../Nicks.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import {_, _args} from "../../I18n.js";
import {MSG, HISTORIC_QUOTES} from "../../consts.js";
import Main from "../../Main.js";
import Log from "../home/Log.js";
import Menu from "../../wgs/Menu.js";
import Msg from "../../wgs/Msg.js";
import Nick from "../../data/Nick.js";
import Quote from "../../data/Quote.js";
import Wrule from "../../wgs/Wrule.js";

const $ = e => Ui.$(e);

const INIT = "init";
const DISABLED = "disabled";
const EDIT = "edit";

function textAreaHeader () {
  return $("td").klass("frame").text(
    _("Date") + ":" +
    _("Open") + ":" +
    _("CloseN") + ":" +
    _("Max") + ":" +
    _("Min") + ":" +
    _("Vol") + ":" +
    _("State")
  );
}

function msgs (msg) {
  switch (msg) {
  case "quotes": return _("Some quotes were or should be corrected");
  case "number": return _args(
    _("Quotes number is different to %0"), String(HISTORIC_QUOTES)
  );
  case "io": return _("Data file not found");
  case "syntax": return _("Syntax error");
  case "empty": return _("Data file is empty");
  case "model": return _("Nick model data are wrong");
  }
  throw new Error(`Unknown message '${msg}'`);
}

/** Nick page -> Editor. */
export default class Editor {

  /**
   * @param {!Nicks} nicks
   * @param {number} nickId Nick to edit
   */
  constructor (nicks, nickId) {
    this._nicks = nicks;
    this._nickId = nickId;
    this._nickId2 = nicks._model;
    this._nick = null;

    this._qLeftMenuSt = INIT;
    this._qRightMenuSt = INIT;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._modNick = Ui.field("modBt").style("width:100px");
    this._modButton = $("button").att("id", "modBt").text(_("Modify"));

    this._serversDiv = $("div");
    this._testDiv = $("div").style("text-align:center");
    this._testSpan = $("span");

    this._quotesDiv = $("div");
    this._qLeftMenu = new Menu(false);
    this._qRightMenu = new Menu(false);
    this._leftArea = $("textarea").att("spellcheck", false)
      .att("rows", 25).att("cols", 60);
    this._rightArea = $("textarea").att("spellcheck", false)
      .att("rows", 25).att("cols", 60)
      .disabled(true);
    this._editBt = $("button").text("Edit")
      .on("click", this.qEdit.bind(this));
    this._resultSpan = $("span");
    this._editCancelBt = $("button").text("Cancel")
      .on("click", this.qEditCancel.bind(this));
    this._modifyBt = $("button").text("Modify")
      .on("click", this.qModify.bind(this));
    this._splitBt = $("button").text("Split")
      .on("click", this.splitMenu.bind(this));

    this._splitMul = Ui.field("splitSee").style("width:40px").value("1");
    this._splitSee = $("button").att("id", "splitSee")
      .style("width:75px").text(_("See"))
      .on("click", this.splitSee.bind(this));
    this._splitCancel = $("button").text("Cancel")
      .on("click", this.splitCancel.bind(this));
    this._splitAccept = $("button").text("Accept")
      .on("click", this.splitAccept.bind(this));

    this._msg = new Msg();
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  get wg () {
    return $("div")
      .add(this._msg.wg)
      .add(Wrule.mkBig(_("Servers")))
      .add(this._testDiv)
      .add(this._serversDiv)
      .add(Wrule.mkBig(_("Quotes")))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td")
            .add($("table").style("width:100%")
              .add($("tr").add($("td").add(this._qLeftMenu.wg)))
              .add($("tr").add(textAreaHeader()))
              .add($("tr").add($("td").add(this._leftArea)))))
          .add($("td")
            .add($("table").style("width:100%")
              .add($("tr").add($("td").add(this._qRightMenu.wg)))
              .add($("tr").add(textAreaHeader()))
              .add($("tr").add($("td").add(this._rightArea)))))))
    ;
  }

  /** @return {void} */
  show () {
    this._nicks.entryDiv.removeAll()
      .add($("table").klass("home")
        .add($("tr").add($("td").add(this._modNick)))
        .add($("tr").add($("td").style("text-align:center")
          .add(this._modButton))));
    this._nicks.view.removeAll()
      .add(this.wg);
    this.updateName();
    this.updateServers();
    this.updateQuotes();
    this._modNick.e.focus();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  async updateName () {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "nick",
      "id": this._nickId
    };
    const rp = await Main.client.send(rq);
    if (rp["nick"] === null) {
      alert(_args(_("Nick with id %0 not found"), String(this._nickId)));
      await this._nicks.setNickSelId(-1);
      location.assign("");
      return;
    }
    /** type {!Nick} */
    const nick = Nick.fromJs(rp["nick"]);
    this._nick = nick;

    const rq2 = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "quotes",
      "name": nick.name
    };
    const rp2 = await Main.client.send(rq2);
    const quotes = rp2["quotes"].map(e => Quote.fromJs(e));
    const nerrs = quotes.reduce((acc, e) => e.error ? ++acc : acc, 0);

    this._modNick.value(nick.name);
    this._modButton.on("click", () => {
      this._nicks.modifyNick(this._modNick.value().trim(), nick.name);
    });
    this._nicks.title.text(`${nick.name} [${quotes.length}](${nerrs})`);
  }

  /** @private */
  async updateServers () {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "serverCodes",
      "id": this._nickId
    };
    const rp = await Main.client.send(rq);
    const sIdNameCodes = rp["sIdNameCodes"];

    this._testDiv.removeAll()
      .add($("table").att("align", "center").add($("tr")
        .add($("td").style("vertical-align:middle")
          .add(Ui.link(() => this.testServers(sIdNameCodes)).klass("link")
            .text(_("Test"))))
        .add($("td"))
        .add($("td")
          .add(this._testSpan.removeAll().add(Ui.img("unknown"))))))
    ;

    const tds = sIdNameCodes.map(inc => {
      const field = $("input").att("type", "text").style("width:125px")
        .value(inc[2]);
      field.on("change", () => this.updateCode(inc[0], field.value().trim()));
      return $("td").style("text-align:center")
        .add($("span").html(`${inc[1]}<br>`))
        .add(field);
    });
    const trs = [];
    let tr = $("tr");
    let i = 0;
    tds.forEach(td => {
      if (i === 0) {
        tr = $("tr");
      }
      tr.add(td);
      ++i;
      if (i === 5) {
        trs.push(tr);
        i = 0;
      }
    });
    if (i !== 0) {
      for (let j = i; j < 5; ++j) {
        tr.add($("td"));
      }
      trs.push(tr);
    }
    this._serversDiv.removeAll()
      .add($("table").klass("frame").att("align", "center").adds(trs));
  }

  /** @private */
  async updateCode (serverId, code) {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "setCode",
      "serverId": serverId,
      "nickId": this._nickId,
      "code": code
    };
    await Main.client.send(rq);
  }

  /** @private */
  async testServers (sIdNameCodes) {
    let ok = true;
    for (let i = 0; i < sIdNameCodes.length; ++i) {
      const rq = {
        "module": "sys",
        "source": "servers/Download", // Reuse callback
        "rq": "historicTest",
        "serverId": sIdNameCodes[i][0],
        "nickId": this._nickId
      };
      this._testSpan.removeAll().html(sIdNameCodes[i][1]);
      const rp = await Main.client.sendLongRun(rq);
      ok = ok && rp["ok"];
    }
    this._testSpan.removeAll().add(Ui.img(ok ? "well" : "error"));
  }

  /** @private */
  async updateQuotes () {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "quotes2",
      "nickId": this._nickId,
      "nickId2": this._nickId2
    };
    const rp = await Main.client.send(rq);
    /** @type {string} */
    const qs1 = rp["qs1"];
    /** @type {number} */
    const err = rp["err"];
    /** @type {string} */
    const msg = rp["msg"];
    /** @type {string} */
    const qs2 = rp["qs2"];
    /** @type {!Array<!Nick>} */
    const nicks = rp["nicks"].map(e => Nick.fromJs(e));
    nicks.sort((n1, n2) => n1.name > n2.name ? 1 : -1);

    let menu = this._qLeftMenu;
    menu.reset();
    menu.addLeft(this._editBt);
    menu.addLeft($("span").html("&nbsp;"));
    menu.addLeft(this._resultSpan);

    menu.addRight(this._modifyBt);
    menu.addRight($("span").html("&nbsp;"));
    menu.addRight(this._editCancelBt);

    if (this._qLeftMenuSt === INIT) {
      this._editBt.disabled(false);
      this._modifyBt.disabled(true);
      this._editCancelBt.disabled(true);
      this._leftArea.disabled(true);
    } else if (this._qLeftMenuSt === EDIT) {
      this._editBt.disabled(true);
      this._modifyBt.disabled(false);
      this._editCancelBt.disabled(false);
      this._leftArea.disabled(false);
    } else {
      this._editBt.disabled(true);
      this._modifyBt.disabled(true);
      this._editCancelBt.disabled(true);
      this._leftArea.disabled(true);
    }

    menu = this._qRightMenu;
    menu.reset();
    if (this._qRightMenuSt === INIT || this._qRightMenuSt === DISABLED) {
      const sel = Ui.select("nks", nicks.map(n =>
        (n.id === this._nickId2 ? "+" : "") + n.name
      ));
      sel.on("change", () => this.setRightArea(nicks[sel.e.selectedIndex]));
      menu.addLeft(sel);
      menu.addRight(this._splitBt.disabled(this._qRightMenuSt === DISABLED));
    }

    this._resultSpan.removeAll().add(
      Ui.link(() => Log.open()).add(
        err === MSG.WARNING
          ? Ui.img("warning").att("title", msgs(msg))
          : err === MSG.ERROR
            ? Ui.img("error").att("title", msgs(msg))
            : Ui.img("well")
      ));
    this._leftArea.value(qs1);
    this._rightArea.value(qs2);
  }

  /** @private */
  async qEdit () {
    this._qLeftMenuSt = EDIT;
    this._qRightMenuSt = DISABLED;
    await this.updateQuotes();
  }

  /** @private */
  async qEditCancel () {
    this._qLeftMenuSt = INIT;
    this._qRightMenuSt = INIT;
    await this.updateQuotes();
  }

  async qModify () {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "setQuotes",
      "nickId": this._nickId,
      "qs": this._leftArea.value().trim()
    };
    const rp = await Main.client.send(rq);
    const err = rp["err"];
    if (err === MSG.ERROR) {
      this._msg.showMsg(err, _("No modification was performed.<br>See Log."));
    } else {
      if (err === MSG.WARNING) {
        this._msg.showMsg(err,
          _("Quotes were modified with corrections.<br>See Log.")
        );
      } else {
        this._msg.showMsg(err, _("Quotes were successfully modified"));
      }
      this.qEditCancel();
    }
  }

  /** @private */
  async setRightArea (nick) {
    const rq = {
      "module": "sys",
      "source": "nicks/Editor",
      "rq": "quotes",
      "name": nick.name
    };
    const rp = await Main.client.send(rq);
    const quotes = rp["quotes"].map(e => Quote.fromJs(e).toString());

    this._rightArea.value(quotes.join("\n"));
  }

  /** @private */
  async splitMenu () {
    await this.setRightArea(this._nick);
    const tx = this._rightArea.value().trim();
    if (tx === "") {
      this._msg.showMsg(
        MSG.ERROR, _args(_("Quotes of '%0' have errors"), this._nick.name)
      );
      this.qEditCancel();
      return;
    }

    this._editBt.disabled(true);
    const menu = this._qRightMenu;
    menu.reset();
    menu.addLeft($("span").text(_("Mult.") + ":"));
    menu.addLeft($("span").html("&nbsp;"));
    menu.addLeft(this._splitMul);
    menu.addLeft($("span").html("&nbsp;"));
    menu.addLeft(this._splitSee);

    menu.addRight(this._splitAccept);
    menu.addRight($("span").html("&nbsp;"));
    menu.addRight(this._splitCancel);

    const el = this._splitMul.e;
    el.setSelectionRange(0, el.value.length);
    el.focus();
  }

  /**
   * @private
   * @return {boolean}
   */
  splitSee () {
    const mul = this._splitMul.value().trim();
    if (isNaN(mul)) {
      alert(_args(_("'%0' is not a number"), mul));
      return false;
    }
    const m = Number(mul);

    const quotes = this._rightArea.value().trim().split("\n")
      .map(s => Quote.fromString(s))
      .map(q => new Quote(
        q.date, q.open * m, q.close * m, q.max * m, q.min * m,
        Math.round(q.vol / m), q.error
      ));

    this._splitAccept.disabled(true);
    this._splitSee.disabled(true);

    this._rightArea.value(quotes.join("\n"));
    return true;
  }

  /** @private */
  splitCancel () {
    const menu = this._qRightMenu;
    menu.reset();
    menu.addRight(this._modifyBt);
    menu.addRight($("span").html("&nbsp;"));
    menu.addRight(this._editCancelBt);
    this.qEditCancel();
  }

  /** @private */
  splitAccept () {
    if (confirm(_args(_("Modify quotes of '%0'?"), this._nick.name))) {
      if (this.splitSee()) {
        this._leftArea.value(this._rightArea.value().trim());
        this.qModify();
      }
    }
  }

}
