// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Tp from "./dmjs/Tp.js";
import Nick from "./data/Nick.js";
import Issue from "./data/Issue.js";
import WserverIds from "./wgs/WserverIds.js";
import Wquotes from "./wgs/Wquotes.js";

const $ = Ui.$;

/** Edit page. */
export default class Edit {
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
    this._modInput = Ui.field("modBt").style("width:100px");

    /** @private */
    this._status = $("span")
      .add(Ui.img("unknown").style("vertical-align:bottom"));
  }

  async modify (nickId, nickName) {
    const data = {
      "page": "edit",
      "rq": "modify",
      "nickId": nickId,
      "nickName": nickName
    };
    const rp = await this._main.client.send(data);
    if (rp["ok"]) {
      this._main.run();
    } else {
      alert(_args(_("'%0' already exists"), nickName));
      this._modInput.e.select();
      this._modInput.e.focus();
    }
  }

  async download (id) {
    this._status.removeAll()
      .add($("img").att("src", "img/wait.gif").style("vertical-align:bottom"));
    const data = {
      "page": "edit",
      "rq": "download",
      "id": id,
    };
    const rp = await this._main.client.send(data);
    if (rp["ok"]) {
      this._main.run();
    } else {
      this._status.removeAll()
        .add(Ui.img("error")
          .style("vertical-align:bottom")
          .att("title", _("Fail downloading quotes"))
        );
    }
  }

  async check (id) {
    const self = this;
    self._status.removeAll()
      .add($("img").att("src", "img/wait.gif").style("vertical-align:bottom"));
    const data = {
      "page": "edit",
      "rq": "check",
      "id": id
    };
    const rp = await self._main.client.send(data);
    const i = Issue.fromJson(rp["issue"]);
    if (i.type === Issue.NONE) {
      self._status.removeAll()
        .add(Ui.img("well").style("vertical-align:bottom"));
    } else {
      self._status.removeAll()
        .add(Ui.link(async () => {
          const data = {
            "page": "nicks",
            "rq": "issue",
            "id": id,
            "menu": Main.issuesPageId
          };
          await self._main.client.send(data);
          self._main.run();
        }).att("title", i.msg)
          .add(Ui.img("error").style("vertical-align:bottom")));
    }

  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  nameEditor (nickId, nickName) {
    const self = this;
    return $("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;")
          .add(self._modInput.value(nickName))
          .add($("span").style("padding-left:5px"))
          .add($("button").att("id", "modBt").text(_("Modify"))
            .on("click", () => {
              self.modify(nickId, self._modInput.value().trim());
            })))
        .add($("td").style("text-align:right;")
          .add(Ui.link(() => {
            self.download(nickId);
          }).klass("link").html(_("Download")))
          .add($("span").html(" · "))
          .add(Ui.link(() => {
            self.check(nickId);
          }).klass("link").html(_("Check")))
          .add($("span").html(" · "))
          .add(self._status)))
    ;
  }

  /** @private */
  show2 (nickId, nickName, modelId, serversIdCode, quotes, modelQuotes, nicks) {
    const qArray = quotes.trim().split("\n");
    const totalLen = qArray.length;
    const errorLen = qArray.filter(n => n.trim().endsWith(":true")).length;
    const w = $("div")
      .add($("h2").style("text-align: center;").html(
        `${nickName} <small><small>[${totalLen}](${errorLen})</big></small>`
      ))
      .add($("hr"))
      .add(this.nameEditor(nickId, nickName))
      .add(new WserverIds(this._main, serversIdCode, nickId).wg())
    ;

    if (quotes !== "") {
      w.add(new Wquotes(
        this._main, nickId, modelId, nicks, quotes, modelQuotes
      ).wg());
    }

    this._main.dom.show(Main.editPageId, w);
    this._modInput.e.focus();
  }

  /** @private */
  showEmpty () {
    const w = $("table").att("align", "center")
      .add($("tr")
        .add($("td").klass("frame4").html(_("No nick was selected"))))
    ;
    this._main.dom.show(Main.editPageId, w);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "page": "edit",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);
    const nickName = rp["name"];
    if (nickName === "") {
      this.showEmpty();
    } else {
      const nickId = rp["id"];
      const modelId = rp["modelId"];
      const serversIdCode = rp["serversIdCode"]
        .map(ic => new Tp(ic[0], ic[1]));
      const quotes = rp["quotes"];
      const modelQuotes = rp["modelQuotes"];
      const nicks = rp["nicks"].map(n => Nick.fromJson(n));
      this.show2(
        nickId, nickName, modelId, serversIdCode, quotes, modelQuotes, nicks
      );
    }
  }
}

