// Copyright 16-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Main from "../Main.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
// eslint-disable-next-line
import Nick from "../data/Nick.js";
import Issue from "../data/Issue.js";
import Wrule from "./Wrule.js";

const $ = Ui.$;

/** Edit page. */
export default class Wquotes {
  /**
   * @param {!Main} main Main
   * @param {string} modelId
   * @param {string} nickId
   * @param {!Array<!Nick>} nicks
   * @param {string} modelQuotes
   * @param {string} quotes
   */
  constructor (main, nickId, modelId, nicks, quotes, modelQuotes) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._modelId = modelId;
    this._nickId = nickId;
    this._nicks = nicks.sort((n1, n2) => n1.nick.localeCompare(n2.nick));
    this._quotes = quotes;

    this._splitDiv = $("div");
    this._editor1 = this.editor(quotes).disabled("true");
    this._editor2 = this.editor(modelQuotes).disabled("true");
  }

  async selAux (nickId) {
    const data = {
      "source": "wquotes",
      "rq": "nickQuotes",
      "id": nickId,
    };
    const rp = await this._main.client.send(data);
    this._editor2.value(rp["quotes"]);
  }

  async modify () {
    const qs = this._editor1.value()
      .trim()
      .split("\n").map(l => {
        const i = l.indexOf("%");
        if (i !== -1) {
          l = l.substring(0, i);
        }
        return l.trim();
      })
      .filter(l => l !== "")
      .join("\n")
    ;
    const data = {
      "source": "wquotes",
      "rq": "modifyQuotes",
      "id": this._nickId,
      "quotes": qs,
    };
    const rp = await this._main.client.send(data);
    const issue = Issue.fromJson(rp["issue"]);
    if (issue.type === Issue.NONE) {
      this._editor1.value(rp["quotes"]);
      alert(_("Quotes updated"));
    } else if (issue.type === Issue.EXISTS) {
      alert(_("Wrong data"));
    } else if (confirm(
      _args(_("There is an issue:\n%0\nForce modification?"), issue.msg)
    )) {
      const data = {
        "source": "wquotes",
        "rq": "modifyForce",
        "id": this._nickId,
        "quotes": qs,
      };
      const rp = await this._main.client.send(data);
      if (rp["ok"]) {
        alert("Quotes updated");
      } else {
        alert("It was not posible to update quotes (wrong data)");
      }
    }
  }

  async check () {
    const qs = this._editor1.value()
      .trim()
      .split("\n").map(l => {
        const i = l.indexOf("%");
        if (i !== -1) {
          l = l.substring(0, i);
        }
        return l.trim();
      })
      .filter(l => l !== "")
      .join("\n")
    ;
    const data = {
      "source": "wquotes",
      "rq": "checkQuotes",
      "id": this._nickId,
      "quotes": qs,
    };
    const rp = await this._main.client.send(data);
    const issue = Issue.fromJson(rp["issue"]);
    this._editor1.value(rp["quotes"]);
    if (issue.type === Issue.NONE) {
      alert(_("No issues detected"));
    } else if (issue.type === Issue.EXISTS) {
      alert(_("Wrong data"));
    } else {
      alert(issue.msg);
    }
  }

  splitQs (mul) {
    if (mul === 1) {
      return this._quotes;
    }
    return this._quotes.split("\n").map(q => {
      const parts = q.split(":");
      return parts[0] + ":" +
        new Dec(Number(parts[1]) * mul, 4).toString() + ":" +
        new Dec(Number(parts[2]) * mul, 4).toString() + ":" +
        new Dec(Number(parts[3]) * mul, 4).toString() + ":" +
        new Dec(Number(parts[4]) * mul, 4).toString() + ":" +
        parts[5] + ":" +
        parts[6]
      ;
    }).join("\n");
  }

  async split (qs) {
    const data = {
      "source": "wquotes",
      "rq": "modifyForce",
      "id": this._nickId,
      "quotes": qs,
    };
    const rp = await this._main.client.send(data);
    if (rp["ok"]) {
      this._splitDiv.removeAll();
      this._main.run();
    } else {
      alert(_("Quotes have wrong format"));
    }
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  splitBts () {
    const self = this;
    const mult = Ui.field("seeBt").style("width:50px").value("1");
    const seeBt = $("button")
      .att("id", "seeBt")
      .text(_("See"))
      .setStyle("width", "70px")
    ;
    const cancelBt = $("button").text(_("Cancel"));
    const acceptBt = $("button").text(_("Accept"));

    seeBt.on("click", () => {
      const m = mult.value().trim();
      if (Dec.isNumber(m)) {
        self._editor2.value(self.splitQs(Number(m)));
      } else {
        alert(`${m} is not a valid number`);
      }
    });
    cancelBt.on("click", () => {
      self._splitDiv.removeAll();
      self._main.run();
    });
    acceptBt.on("click", () => {
      const m = mult.value().trim();
      if (!Dec.isNumber(m)) {
        alert(`${m} is not a valid number`);
      } else if (confirm(_("Save split?"))) {
        self.split(self.splitQs(Number(m)));
      }
    });

    return $("table").style("width: 100%")
      .add($("tr")
        .add($("td")
          .add($("span").text(_("Mult.") + ": "))
          .add(mult)
          .add($("span").html("&nbsp;&nbsp;"))
          .add(seeBt))
        .add($("td").style("text-align:right;")
          .add(cancelBt)
          .add($("span").html("&nbsp;&nbsp;"))
          .add(acceptBt)))
    ;
  }

  buttonsTr () {
    const self = this;
    const ids = [];
    const nicks = [];
    for (const nick of self._nicks) {
      ids.push(nick.id);
      nicks.push((this._modelId === nick.id ? "+" : "") + nick.nick);
    }
    const editBt = $("button").text(_("Edit"));
    const doneBt = $("button").text(_("Done")).disabled(true);
    const modifyBt = $("button").text(_("Modify")).disabled(true);
    const nickSelect = Ui.select("nicks", nicks);
    const splitBt = $("button").text(_("Split"));

    editBt.on("click", () => {
      doneBt.disabled(false);
      modifyBt.disabled(false);
      this._editor1.disabled(false);
      editBt.disabled(true);
      splitBt.disabled(true);
    });
    doneBt.on("click", () => {
      /*
      doneBt.disabled(true);
      modifyBt.disabled(true);
      this._editor1.disabled(true);
      editBt.disabled(false);
      splitBt.disabled(false);
      */
      this._main.run();
    });
    modifyBt.on("click", () => {
      self.modify();
    });
    nickSelect.on("change", () => {
      self.selAux(ids[nickSelect.e.selectedIndex]);
    });
    splitBt.on("click", () => {
      editBt.disabled(true);
      self._splitDiv.removeAll().add(self.splitBts());
      self._editor1.value(self._quotes);
      self._editor2.value(self._quotes);
    });

    self._splitDiv.add($("table").style("width: 100%")
      .add($("tr")
        .add($("td").add(nickSelect))
        .add($("td").style("text-align:right;").add(splitBt))));

    return $("tr")
      .add($("td").add(editBt))
      .add($("td").style("text-align:right;")
        .add(Ui.link(() => {
          self.check();
        }).klass("link").text(_("Check")))
        .add($("span").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
        .add(doneBt)
        .add($("span").html("&nbsp;&nbsp;"))
        .add(modifyBt))
      .add($("td")
        .add(self._splitDiv))
    ;

  }

  editorHeader () {
    return $("div").klass("frame").text(
      _("Date") + ":" +
      _("Open") + ":" +
      _("Close") + ":" +
      _("Max") + ":" +
      _("Min") + ":" +
      _("Vol") + ":" +
      _("Status") + ":");
  }

  editor (text) {
    const output = $("textarea").att("rows", 25).att("cols", 60);
    output.value(text);
    return output;
  }

  wg () {
    return $("div")
      .add(Wrule.mkBig(_("Quotes")))
      .add($("table").att("align", "center")
        .add(this.buttonsTr())
        .add($("tr")
          .add($("td").att("colspan", 2)
            .add(this.editorHeader())
            .add($("div").style("height: 4px;"))
            .add(this._editor1))
          .add($("td")
            .add(this.editorHeader())
            .add($("div").style("height: 4px;"))
            .add(this._editor2))
        )
      )
    ;
  }
}
