// Copyright 27-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Nick editor page.
**/

import Maybe from "../../../dmjs/Maybe.js";
import Ui from "../../../dmjs/Ui.js";
import ModalBox from "../../../dmjs/ModalBox.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import Nick from "../../../data/Nick.js";  //eslint-disable-line
import {Menu, MenuEntry} from "../../../dmjs/Menu.js";
import Msg from "../../../wgs/Msg.js";
import Wrule from "../../../wgs/Wrule.js";
import {_, _args} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Quote from "../../../data/Quote.js";

const $ = e => Ui.$(e);

/**
    @return !Domo
**/
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

/**
    Nick editor page.
**/
export default class Editor {
  /**
      @param {!Domo} inputDiv
      @param {!Domo} menu2
      @param {!Domo} bodyDiv
      @param {!Array<!Nick>} nicks
      @param {!Nick} nick
      @param {!Nick} nickModel
      @param {!Array<!Quote>} quotes Company quotes.
      @param {number} manuals Number of manual modifications in company quotes.
      @param {!Array<!Quote>} mquotes Model quotes.
      @param {!Array<!Array<?>>} sIdNameCodes Array of [serverId, serverName,
                                              companyCode]
  **/
  constructor (
    inputDiv,
    menu2,
    bodyDiv,
    nicks,
    nick,
    nickModel,
    quotes,
    manuals,
    mquotes,
    sIdNameCodes
  ) {
    this._inputDiv = inputDiv;
    this._menu2 = menu2;
    this._bodyDiv = bodyDiv;
    nicks.sort((n1, n2) => n1.name > n2.name ? 1 : -1);
    this._nicks = nicks;
    this._nick = nick;
    this._nickModel = nickModel;
    this._quotes = quotes;
    this._manuals = manuals;
    this._mquotes = mquotes;
    this._sIdNameCodes = sIdNameCodes;

    this._nickIn = Ui.field("inBt").style("width:100px");

    this._serverTestSpan = $("span");

    this._editBt = $("button")
      .text(_("Edit"))
      .on("click", () => { this.qEdit() });
    this._cancelBt = $("button")
      .text(_("Cancel"))
      .disabled(true)
      .on("click", () => { this.qCancel() });
    this._modifyBt = $("button")
      .text(_("Modify"))
      .disabled(true)
      .on("click", () => { this.qModify() })
    ;
    this._leftArea = $("textarea")
      .att("spellcheck", false)
      .att("rows", 25)
      .att("cols", 60)
      .disabled(true)
    ;


    this._splitDiv = $("div");
    this._splitBt = $("button")
      .text(_("Split"))
      .on("click", () => { this.splitMenu() });
    this._splitMul = Ui.field("splitSee")
      .style("width:40px")
      .value("1");
    this._splitSee = $("button")
      .att("id", "splitSee")
      .style("width:75px")
      .text(_("See"))
      .on("click", () => { this.splitSee() });
    this._splitCancel = $("button")
      .text(_("Cancel"))
      .on("click", () => { this.splitCancel() });
    this._splitAccept = $("button")
      .text(_("Accept"))
      .on("click", () => { this.splitAccept() });
    this._rightArea = $("textarea")
      .att("spellcheck", false)
      .att("rows", 25)
      .att("cols", 60)
      .disabled(true)
    ;

    this._msgWait = $("div");

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    const nick = this._nick;
    const quotes = this._quotes;

    this._nickIn.value(nick.name);
    this._inputDiv
      .removeAll()
      .add($("table")
        .klass("home")
        .add($("tr")
          .add($("td")
            .add(this._nickIn)))
        .add($("tr")
          .add($("td")
            .style("text-align:center")
            .add($("button")
              .att("id", "inBt")
              .text(_("Modify"))
              .on("click", () => {
                this.modifyNick(this._nickIn.value().trim());
              })))))
    ;

    const lopts = [
      new MenuEntry(
        Maybe.nothing,
        $("span")
          .style("font-size: 24px;")
          .text(`${nick.name} [${quotes.length}](${this._manuals})`)
      )
    ];
    const ropts = [
      Menu.toption("download", _("Download"), () => this.download()),
      Menu.separator(),
      Menu.toption("test", _("Test"), () => this.test())
    ];
    const menu = new Menu(lopts, ropts, "");
    this._menu2
      .removeAll()
      .add(menu.wg)
    ;

    this._leftArea.text(this._quotes.map(q => q.toString()).join("\n"));
    this._rightArea.text(this._mquotes.map(q => q.toString()).join("\n"));
    this._bodyDiv
      .removeAll()
      .add(this._msgWait)
      .add(Wrule.mkBig(_("Servers")))
      .add(this.serversDiv())
      .add(Wrule.mkBig(_("Quotes")))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td")
            .add($("table")
              .style("width:100%")
              .add($("tr")
                .add($("td")
                  .add(this.leftMenu().wg)))
              .add($("tr")
                .add(textAreaHeader()))
              .add($("tr")
                .add($("td")
                  .add(this._leftArea)))))
          .add($("td")
            .add($("table")
              .style("width:100%")
              .add($("tr")
                .add($("td")
                  .add(this._splitDiv
                    .removeAll()
                    .add(this.rightMenu().wg))))
              .add($("tr")
                .add(textAreaHeader()))
              .add($("tr")
                .add($("td")
                  .add(this._rightArea)))))))
    ;

  }

  /**
      @private
      @return !Domo
  **/
  serversDiv () {
    const tds = this._sIdNameCodes.map(inc => {
      const field = $("input")
        .att("type", "text")
        .style("width:125px")
        .value(inc[2]);
      field.on("change", () => this.updateCode(inc[0], field.value().trim()));
      return $("td")
        .style("text-align:center")
        .add($("span")
          .html(`${inc[1]}<br>`))
        .add(field)
      ;
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

    return $("div")
      .add($("table")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .style("vertical-align:middle")
            .add(Ui.link(() => this.serverTests())
              .klass("link")
              .text(_("Test"))))
          .add($("td"))
          .add($("td")
            .add(this._serverTestSpan
              .removeAll()
              .add(Ui.img("unknown"))))))
      .add($("table")
        .klass("frame")
        .att("align", "center")
        .adds(trs))
    ;
  }

  /**
      @private
      @return !Menu
  **/
  leftMenu () {
    const lopts = [
      new MenuEntry(Maybe.nothing, this._editBt),
    ];
    const ropts = [
      new MenuEntry(Maybe.nothing, this._cancelBt),
      new MenuEntry(Maybe.nothing, $("span").html("&nbsp;")),
      new MenuEntry(Maybe.nothing, this._modifyBt),
    ];
    return new Menu(lopts, ropts, "");
  }

  /**
      @private
      @return !Menu
  **/
  rightMenu () {
    const sel = Ui.select("nks", this._nicks.map(n =>
      (n.id === this._nickModel.id ? "+" : "") + n.name
    ));
    sel.on("change", () => {
      this.setRightArea(this._nicks[sel.e.selectedIndex]);
    });
    const lopts = [
      new MenuEntry(Maybe.nothing, sel)
    ];
    const ropts = [
      new MenuEntry(Maybe.nothing, this._splitBt)
    ];
    return new Menu(lopts, ropts, "");
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
      @param {string} nickName
      @return none
  **/
  async modifyNick (nickName) {
    if (nickName === "") {
      Msg.error(_("Nick name is missing"), () => { this.view() });
      return;
    }
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "modifyNick",
      "nickId": this._nick.id,
      "name": nickName
    });

    if (!rp["ok"]) {
      Msg.error(
        Cts.failMsg,
        () => {
          Editor.mk(
            this._inputDiv, this._menu2, this._bodyDiv,
            this._nicks, this._nick, this._nickModel
          );
        }
      );
    } else {
      const nick = this._nick;
      Msg.ok(
        Cts.okMsg,
        () => {
          Editor.mk(
            this._inputDiv, this._menu2, this._bodyDiv, this._nicks,
            new Nick(nick.id, nickName, nick.isSel),
            this._nickModel
          );
        }
      );
    }
  }

  /**
      @private
      @param {number} serverId
      @param {string} code
      @return !Promise<void>
  **/
  async updateCode (serverId, code) {
    if (code === "") {
      Msg.error(_("Nick code is missing"), () => { this.view() });
      return;
    }
    await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "updateCode",
      "serverId": serverId,
      "nickId": this._nick.id,
      "code": code
    });
  }

  /**
      @private
      @return void
  **/
  async download () {
    this.setWait(_("Downloading..."));
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "download",
      "nickId": this._nick.id
    });
    this.setWait("");

    if (rp["result"] === "error") {
      Msg.error(_("Some error was found.<br>See Log."));
      return;
    }
    if (rp["result"] === "warning") {
      Msg.info(_("Some quote was modified.<br>See Log."));
    } else {
      Msg.ok(_("Download ok."));
    }

    Editor.mk(
      this._inputDiv, this._menu2, this._bodyDiv,
      this._nicks, this._nick, this._nickModel
    );
  }

  /**
      @private
      @return void
  **/
  async test () {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "test",
      "qs": this._leftArea.value().trim()
    });

    if (rp["result"] === "error") {
      Msg.error(_("Some error was found.<br>See Log."));
      return;
    }
    if (rp["result"] === "warning") {
      Msg.info(_("Some quote needs modification.<br>See Log."));
    } else {
      Msg.ok(_("Test ok."));
    }
  }

  /**
      @private
      @return void
  **/
  async serverTests () {
    let ok = true;
    for (const idNameCode of this._sIdNameCodes) {
      this.setWait(idNameCode[2]);

      const rp = await Cts.client.ssend({
        "module": "settings",
        "source": "nicks/editor",
        "rq": "serverTests",
        "serverId": idNameCode[0],
        "nickId": this._nick.id
      });

      ok = ok && rp["ok"];
    }
    this.setWait("");
    this._serverTestSpan.removeAll().add(Ui.img(ok ? "well" : "error"));
  }

  /**
      @private
      @return void
  **/
  qEdit () {
    this._editBt.disabled(true);
    this._cancelBt.disabled(false);
    this._modifyBt.disabled(false);
    this._leftArea.disabled(false);
    this._splitBt.disabled(true);
  }

  /**
      @private
      @return void
  **/
  qCancel () {
    this._editBt.disabled(false);
    this._cancelBt.disabled(true);
    this._modifyBt.disabled(true);
    this._leftArea.disabled(true);
    this._splitBt.disabled(false);
    this.view();
  }

  /**
      @private
      @return void
  **/
  async qModify () {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "qModify",
      "nickId": this._nick.id,
      "qs": this._leftArea.value().trim()
    });

    if (rp["result"] === "error") {
      Msg.error(_("No modification was performed.<br>See Log."));
      return;
    }
    if (rp["result"] === "warning") {
      Msg.info(_("Quotes were modified with corrections.<br>See Log."));
    } else {
      Msg.ok(_("Quotes were successfully modified"));
    }
    Editor.mk(
      this._inputDiv, this._menu2, this._bodyDiv,
      this._nicks, this._nick, this._nickModel
    );
  }

  /**
      @private
      @param {Nick} nick
      @return void
  **/
  async setRightArea (nick) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "getQuotes",
      "nickName": nick.name,
    });
    const qs = rp["quotes"].map(e => Quote.fromJs(e));
    this._rightArea.text(qs.map(q => q.toString()).join("\n"));
  }

  /**
      @private
      @return void
  **/
  splitMenu () {
    const lopts = [
      new MenuEntry(Maybe.nothing, $("span").text(_("Mult.") + ":")),
      new MenuEntry(Maybe.nothing, $("span").html("&nbsp;")),
      new MenuEntry(Maybe.nothing, this._splitMul),
      new MenuEntry(Maybe.nothing, $("span").html("&nbsp;")),
      new MenuEntry(Maybe.nothing, this._splitSee)
    ];
    const ropts = [
      new MenuEntry(Maybe.nothing, this._splitCancel),
      new MenuEntry(Maybe.nothing, $("span").html("&nbsp;")),
      new MenuEntry(Maybe.nothing, this._splitAccept),
    ];
    const menu = new Menu(lopts, ropts, "");

    this._splitDiv
      .removeAll()
      .add(menu.wg)
    ;

    this._rightArea.text(this._leftArea.text());
  }

  /**
      @private
      @return boolean
  **/
  splitSee () {
    const mul = this._splitMul.value().trim();
    if (isNaN(mul)) {
      alert(_args(_("'%0' is not a number"), mul));
      return false;
    }
    const m = Number(mul);

    const quotes = this._leftArea.value().trim().split("\n")
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

  /**
      @private
      @return void
  **/
  splitCancel () {
    Editor.mk(
      this._inputDiv, this._menu2, this._bodyDiv,
      this._nicks, this._nick, this._nickModel
    );
  }

  /**
      @private
      @return void
  **/
  splitAccept () {
    if (confirm(_args(_("Modify quotes of '%0'?"), this._nick.name))) {
      if (this.splitSee()) {
        this._leftArea.value(this._rightArea.value().trim());
        this.qModify();
      }
    }
  }



  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} inputDiv
      @param {!Domo} menu2
      @param {!Domo} bodyDiv
      @param {!Array<!Nick>} nicks
      @param {!Nick} nick
      @param {!Nick} nickModel
      @return !Promise<<Maybe<!Editor>>
  **/
  static async mk (inputDiv, menu2, bodyDiv, nicks, nick, nickModel) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks/editor",
      "rq": "idata",
      "nickId": nick.id,
      "modelId": nickModel.id
    });

    if (!rp["ok"]) {
      Msg.error(Cts.failMsg);
      return Maybe.nothing;
    }

    return Maybe.just(new Editor(
      inputDiv,
      menu2,
      bodyDiv,
      nicks,
      nick,
      nickModel,
      rp["quotes"].map(e => Quote.fromJs(e)),
      rp["manuals"],
      rp["mquotes"].map(e => Quote.fromJs(e)),
      rp["sIdNameCodes"]
    ));
  }
}
