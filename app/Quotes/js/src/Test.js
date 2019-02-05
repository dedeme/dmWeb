// Copyright 29-Jan-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";

const $ = Ui.$;

const COS_NICKS = 0;
const COS_EXTRA = 1;

function err (msg) {
  return msg
    .replace(/_1_/g, _("Ok."))
    .replace(/_2_/g, _("Error reading local quotes"))
    .replace(/_3_/g, _("Bad nick Id in Test/readQuotesSvs"))
    .replace(/_4_/g, _("Extra quote"))
    .replace(/_5_/g, _("Open"))
    .replace(/_6_/g, _("Close"))
    .replace(/_7_/g, _("Maximum"))
    .replace(/_8_/g, _("Minimum"))
    .replace(/_9_/g, _("Volume"))
  ;
}

function mkMenuTd (sel, tx, f) {
  if (sel) {
    return $("td").klass("menu").html(tx);
  }
  return $("td").add(Ui.link(f).klass("link").html(tx));
}

function mkMenu (sel, tx, f) {
  if (sel) {
    return $("span").klass("frame").html(tx);
  }
  return Ui.link(f).klass("link").html(tx);
}

/** Test page. */
export default class Test {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._cosTypeDiv = $("div");

    this._nicksDiv = $("div");

    this._textArea = $("textArea").att("cols", 110).att("rows", 30)
      .att("readOnly", true);

    this._cosType = COS_NICKS;

    this._selNick = "";
  }

  async nickIssues (nickId, f) {
    const rq = {
      "source": "test",
      "rq": "issues",
      "nickId": nickId
    };
    const rp = await this._main.client.send(rq);
    const issues = rp["issues"];
    this._textArea.value(this._textArea.value() + err(issues));
    f();
  }

  async setNicks () {
    const rq = {
      "source": "test",
      "rq": this._cosType === COS_NICKS ? "nicks" : "extra"
    };
    const rp = await this._main.client.send(rq);
    const idNicks = rp["idNicks"].sort((iN1, iN2) => iN1[1] > iN2[1] ? 1 : -1);

    const tb = $("table").klass("frame2").att("align", "center")
      .style("border-collapse : collapse;");
    let n = 0;
    let tr = $("tr");
    idNicks.forEach(iN => {
      const [id, nk] = iN;
      let border = "border-left: solid 1px;";
      if (n > 0 && n % 10 === 0) {
        tb.add(tr);
        tr = $("tr");
        border = "";
      } else if (n === 0) {
        border = "";
      }
      const td = mkMenuTd(this._selNick === nk, nk, () => {
        this._textArea.value("");
        td.removeAll().add(Ui.img("wait.gif"));
        this.nickIssues(id, this.setNicks.bind(this));
        this._selNick = nk;
      }).style("text-align:center;" + border);
      tr.add(td);
      ++n;
    });
    while (n % 10 !== 0) {
      tr.add($("td"));
    }
    tb.add(tr);
    this._nicksDiv.removeAll().add(tb);

    const td = mkMenuTd(this._selNick === "@", _("All nicks"), () => {
      this._textArea.value("");
      td.removeAll().add(Ui.img("wait.gif"));
      this._selNick = "@";
      It.from(idNicks).eachSync(
        (iN) => this.nickIssues.bind(this)(iN[0], () => {}),
        this.setNicks.bind(this)
      );
    }).att("colspan", 10).style("text-align:center;border-top: solid 1px;");
    tb.add($("tr").add(td));
  }

  setCosType () {
    this._cosTypeDiv.removeAll()
      .add(mkMenu(this._cosType === COS_NICKS, _("Nicks"), () => {
        this._cosType = COS_NICKS;
        this.setCosType();
      }))
      .add($("span").html("&nbsp;&nbsp;||&nbsp;&nbsp;"))
      .add(mkMenu(this._cosType === COS_EXTRA, _("Extra"), () => {
        this._cosType = COS_EXTRA;
        this.setCosType();
      }));

    this._selNick = "";
    this.setNicks();

    this._textArea.value("");
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show(
      Main.testPageId,
      $("table").att("align", "center")
        .add($("tr")
          .add($("td").style("text-align:center")
            .add($("h2").html(_("Test")))))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td").style("text-align:center")
            .add(this._cosTypeDiv)))
        .add($("tr")
          .add($("td")))
        .add($("tr")
          .add($("td")
            .add(this._nicksDiv.removeAll())))
        .add($("tr")
          .add($("td")
            .add(this._textArea.value(""))))
    );

    this.setCosType();
  }
}

