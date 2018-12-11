// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Tp from "./dmjs/Tp.js";
import Nick from "./data/Nick.js";
import Issue from "./data/Issue.js";
import WserverIds from "./wgs/WserverIds.js";
import Wquotes from "./wgs/Wquotes.js";
import Wrule from "./wgs/Wrule.js";

const $ = Ui.$;

const translator = (() => {
  const format = (dateFormat, l, dec, sep) => {
    let prev = "";
    return It.range(l.length).reduce("", (s, i) => {
      const ch = l.charAt(i);
      if (prev === "") {
        if (ch <= " ") {
          prev = " ";
          return dateFormat(s) + ":";
        }
        return s + ch;
      }
      if (ch <= " ") {
        if (prev === " ") {
          return s;
        }
        prev = " ";
        return s + ":";
      }
      prev = ch;
      return s + (ch === dec ? "." : ch === sep ? "" : ch);
    });
  };

  const finanzas = (tx) => {
    const ps = format(
      (date) => {
        const ps = date.split("/");
        return ps[2] + ps[1] + ps[0];
      },
      tx, ",", "."
    ).split(":");
    return ps[0] + ":" + ps[1] + ":" + ps[2] + ":" +
      ps[4] + ":" + ps[5] + ":" + ps[6] + ":false";
  };
  const yahoo = (tx) => {
    const months = ["ene.", "feb.", "mar.", "abr.", "may.", "jun.",
      "jul.", "ago.", "sep.", "oct.", "nov.", "dic."];
    months.forEach((m, i) => tx = tx.replace(` ${m} `, `/${i}/`));
    const ps = format(
      (date) => {
        const ps = date.split("/");
        return ps[2] + ps[1] + ps[0];
      },
      tx, ",", "."
    ).split(":");
    return ps[0] + ":" + ps[1] + ":" + ps[4] + ":" +
      ps[2] + ":" + ps[3] + ":" + ps[6] + ":false";
  };
  const invertia = (tx) => {
    const ps = format(
      (date) => {
        const ps = date.split("/");
        return 20 + ps[2] + ps[1] + ps[0];
      },
      tx, ",", "."
    ).split(":");
    return ps[0] + ":" + ps[2] + ":" + ps[1] + ":" +
      ps[4] + ":" + ps[5] + ":" + ps[6] + ":false";
  };
  const ed1 = $("textarea").att("rows", 3).att("cols", 60);
  const ed2 = $("input").att("type", "text").style("width: 100%");

  return $("div")
    .add(Wrule.mkSmall(_("Translator")))
    .add(ed1)
    .add($("table").att("align", "center").style("spacing: 20px;")
      .add($("tr")
        .add($("td").add($("button").text("Finanzas").on("click", () => {
          ed2.value(finanzas(ed1.value())).e.select();
        })))
        .add($("td").add($("button").text("Yahoo").on("click", () => {
          ed2.value(yahoo(ed1.value())).e.select();
        })))
        .add($("td").add($("button").text("Invertia").on("click", () => {
          ed2.value(invertia(ed1.value())).e.select();
        })))))
    .add(ed2)
  ;
})();

/** Issues page. */
export default class Issues {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._nick = $("div").style("text-align:center;");
    this._cause = $("div");
    this._solution = $("div");
  }

  async moreIssues (search) {
    const self = this;
    const data = {
      "source": "issues",
      "rq": "nextIssue"
    };
    const rp = await self._main.client.send(data);
    const nickId = rp["nickId"];
    if (nickId === "") {
      search.removeAll().add($("span").text(_("There are no more issues")));
    } else {
      const data = {
        "source": "wnick",
        "rq": "issue",
        "id": nickId,
        "menu": Main.issuesPageId
      };
      await this._main.client.send(data);
      self._main.run();
    }
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /** @private */
  noNick () {
    const self = this;
    const search = $("div");
    self._nick.removeAll()
      .add($("h2").style("text-align: center;").html(_("Issues")));
    self._cause.removeAll().style("text-align:center;")
      .add($("table").klass("frame").att("align", "center")
        .add($("tr")
          .add($("td").text(_("No nick was selected")))))
      .add($("br"))
      .add(search.add(Ui.link(() => {
        self.moreIssues(search);
      }).klass("link").text(_("Search more issues"))));
    self._solution.removeAll();
  }

  none () {
    const self = this;
    const search = $("div");
    self._cause.removeAll().style("text-align: center;")
      .add($("hr"))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").add(Ui.img("well")))))
      .add($("br"))
      .add(search.add(Ui.link(() => {
        self.moreIssues(search);
      }).klass("link").text(_("Search more issues"))));
    self._solution.removeAll();
  }

  async server (nickId) {
    const self = this;
    const data = {
      "source": "issues",
      "rq": "serverIdCodes",
      "id": nickId
    };
    const rp = await this._main.client.send(data);
    const serverIdCodes = rp["serverIdCodes"]
      .map(ic => new Tp(ic[0], ic[1]));
    self._solution.removeAll()
      .add(new WserverIds(self._main, serverIdCodes, nickId).wg())
      .add($("div").style("text-align:right; padding-right:10px")
        .add($("button").text(_("Done")).on("click", () => {
          self._main.run();
        })))
    ;
  }

  browser (serverLinks) {
    return $("div")
      .add(Wrule.mkSmall(_("Browser")))
      .add($("table").att("align", "center").style("spacing: 20px;")
        .add($("tr")
          .adds(serverLinks.map(sl =>
            $("td").add($("button").text(sl[0]).on("click", () => {
              window.open(sl[1]);
            }))
          ))))
    ;
  }

  async qissues (nickId) {
    const data = {
      "source": "issues",
      "rq": "qissue",
      "id": nickId
    };
    const rp = await this._main.client.send(data);
    const modelId = rp["modelId"];
    const nicks = rp["nicks"].map(n => Nick.fromJson(n));
    const quotes = rp["quotes"];
    const modelQuotes = rp["modelQuotes"];
    const serverLinks = rp["serverLinks"];
    this._solution.removeAll()
      .add(new Wquotes(
        this._main, nickId, modelId, nicks, quotes, modelQuotes
      ).wg())
      .add($("table").att("width", "100%")
        .add($("tr")
          .add($("td").style("width: 10px").add(translator))
          .add($("td").style("vertical-align: top;")
            .add(this.browser(serverLinks)))))
    ;
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "source": "issues",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);
    const id = rp["id"];
    const nick = rp["name"];
    const issue = Issue.fromJson(rp["issue"]);

    const w = $("div")
      .add(this._nick)
      .add(this._cause)
      .add(this._solution)
    ;

    if (id === "" || nick === "") {
      this.noNick();
    } else {
      this._nick.removeAll()
        .add($("h2").style("text-align: center;").html(nick));

      if (issue.type === Issue.NONE) {
        this.none();
      } else {
        this._cause.removeAll()
          .add($("hr"))
          .add($("table").att("align", "center")
            .add($("tr")
              .add($("td")
                .add(Ui.img("error")))
              .add($("td").klass("frame").html(issue.msg))));

        switch (issue.type) {
        case Issue.SERVER:
          this.server(id);
          break;
        case Issue.EMPTY:
          this._solution.removeAll();
          break;
        default:
          this.qissues(id);
        }
      }
    }

    this._main.dom.show(Main.issuesPageId, w);
  }
}

