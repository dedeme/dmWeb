// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Tp from "./dmjs/Tp.js";
import Issue from "./data/Issue.js";
import WserverIds from "./wgs/WserverIds.js";

const $ = Ui.$;

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
    this._aux = $("div");
  }

  async moreIssues (search) {
    const self = this;
    const data = {
      "page": "issues",
      "rq": "nextIssue"
    };
    const rp = self._main.client.send(data);
    const nickId = rp["nickId"];
    if (nickId === "") {
      search.removeAll().add($("span").text(_("There are no more issues")));
    } else {
      const data = {
        "page": "nicks",
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
  empty () {
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
    self._aux.removeAll();
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
    self._aux.removeAll();
  }

  async server (nickId) {
    const self = this;
    const data = {
      "page": "issues",
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
    self._aux.removeAll();
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "page": "issues",
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
      .add(this._aux)
    ;

    if (id === "" || nick === "") {
      this.empty();
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
              .add($("td").klass("frame").html(issue.cause))));

        switch (issue.type) {
        case Issue.SERVER:
          this.server(id);
          break;
        default:
          alert("Without implementation");
        }
      }
    }

    this._main.dom.show(Main.issuesPageId, w);
  }
}

