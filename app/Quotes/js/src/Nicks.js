// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Nick from "./data/Nick.js";
import Issue from "./data/Issue.js";
import Wnick from "./wgs/Wnick.js";

const $ = Ui.$;

/** Nicks - Extra pages. */
export default class Nicks {
  /**
   * @param {!Main} main Main
   * @param {boolean} isExtra If page is extra
   */
  constructor (main, isExtra) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /**
     * @private
     * @type {boolean}
     */
    this._isExtra = isExtra;

    /**
     * @private
     * @type {!Array<!Wnick>}
     */
    this._wnicks = [];

    /**
     * @private
     * @type {number}
     */
    this._total = 0;

    /**
     * @private
     * @type {number}
     */
    this._ibex = 0;

    /**
     * @private
     * @type {number}
     */
    this._sel = 0;
  }

  download () {
    for (const wnick of this._wnicks) {
      wnick.wait();
      const wait = new Promise(resolve => {
        setTimeout(async () => {
          const data = {
            "source": "edit",
            "rq": "download",
            "id": wnick.id,
          };
          const rp = await this._main.client.send(data);
          wnick.download(rp["error"]);
          resolve();
        }, 200);
      });
      (async () => {
        await wait;
      })();
    }
  }

  async check () {
    for (const wnick of this._wnicks) {
      wnick.wait();
      const id = wnick.id;
      const data = {
        "source": "edit",
        "rq": "check",
        "id": id
      };
      const rp = await this._main.client.send(data);
      const i = Issue.fromJson(rp["issue"]);
      if (i.type === Issue.NONE) {
        wnick.check(true, "");
      } else {
        wnick.check(false, i.msg);
      }
    }
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  sep () {
    return $("span").style("padding-left:5px");
  }

  emptyList () {
    return $("table").att("align", "center")
      .add($("tr")
        .add($("td").klass("frame").html(_("Without nicks"))))
    ;
  }

  fullEntries () {
    const len = this._wnicks.length;
    const cols = 5;
    const rows = Math.floor((len - 1) / cols) + 1;
    return $("table").att("width", "100%").klass("frame")
      .adds([...It.range(rows).map(row =>
        $("tr").adds([...It.range(cols).map(col => {
          const ix = col * rows + row;
          if (ix >= len) {
            return $("td");
          }
          return $("td").add(this._wnicks[ix].wg());
        })]))]);
  }

  head (input) {
    const addNick = async () => {
      const nick = input.value().trim();
      const data = {
        "source": "nicks",
        "rq": "new",
        "nick": nick,
        "isExtra": this._isExtra
      };
      const rp = await this._main.client.send(data);
      if (rp["ok"]) {
        this._main.run();
      } else {
        alert(_args(_("'%0' already exists in nicks or extra"), nick));
        input.e.focus();
      }
    };
    const self = this;
    return $("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;white-space:nowrap;")
          .add(input)
          .add(self.sep())
          .add($("button")
            .style("width: 150px")
            .att("id", "newBt")
            .html(_("New nick"))
            .on("click", addNick))
          .add($("span")
            .html(
              "&nbsp;&nbsp;Total: " + String(this._total) +
              " (Ibex: " + String(this._ibex) +
              ", Sel: " + String(this._sel) + ")")))
        .add($("td").style("text-align:right;")
          .add(Ui.link(() => {
            self.download();
          }).klass("link").html(_("Download")))
          .add($("span").html(" · "))
          .add(Ui.link(() => {
            self.check();
          }).klass("link").html(_("Check")))))
    ;
  }

  /** @private */
  show2 () {
    const newInput = Ui.field("newBt").style("width:100px");
    const w = $("div")
      .add(this.head(newInput))
      .add(this._wnicks.length === 0 ? this.emptyList() : this.fullEntries())
    ;
    this._main.dom.show(
      this._isExtra ? Main.extraPageId : Main.nicksPageId,
      w
    );
    newInput.e.focus();
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "source": "nicks",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);
    const model = rp["model"];
    const nicks = rp["nicks"]
      .map(n => Nick.fromJson(n))
      .filter(n => n.isExtra === this._isExtra)
      .sort((n1, n2) => n1.nick.localeCompare(n2.nick))
    ;
    this._total = 0;
    this._ibex = 0;
    this._sel = 0;
    nicks.forEach(nk => {
      ++this._total;
      if (nk.isIbex) {
        ++this._ibex;
      }
      if (nk.isSel) {
        ++this._sel;
      }
    });
    this._wnicks = nicks.map(n => new Wnick(this._main, model, n));
    this.show2();
  }
}

