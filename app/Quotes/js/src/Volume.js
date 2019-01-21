// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Dec from "./dmjs/Dec.js";

const $ = Ui.$;

const nickTd = $("td").style("text-align:center").html("---");
const body = $("div");

/** Servers page. */
export default class Volume {
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
    this._byNick = false;

    /** @private */
    this._reverse = false;

  }

  formatN (n) {
    const dec = new Dec(n, 0);
    return this._main.model["lang"] === "es" ? dec.toEu() : dec.toEn();
  }

  mkAll (cos) {
    return cos.map(row =>
      $("tr")
        .add($("td")
          .add(Ui.img(
            row[0] === "sel" ? "flag1"
              : row[0] === "in" ? "transOn"
                : "transOff"
          )))
        .add($("td").style("text-align:left;")
          .html(row[1]))
        .add($("td").style("text-align:right;")
          .html(this.formatN(row[2])))
    );
  }

  setBody (cos) {
    const table = () => $("table").klass("frame").att("align", "center");
    body.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("text-align:center")
            .add($("div").klass("head").html(_("All<br>Companies")))
            .add($("div")
              .add($("span").html(_("Order by") + ": "))
              .add(Ui.link(() => {
                this._byNick = true;
                this._reverse = false;
                this.setBody(cos);
              }).klass("link").html(_("Nick")))
              .add($("span").html(" | "))
              .add(Ui.link(() => {
                this._byNick = false;
                this._reverse = false;
                this.setBody(cos);
              }).klass("link").html(_("Vol."))))
            .add($("div")
              .add(Ui.link(() => {
                this._reverse = !this._reverse;
                this.setBody(cos);
              }).klass("link").html(_("Reverse"))))
            .add(table().adds(this.mkAll(cos))))
          .add($("td").style("vertical-align: top;")
            .add($("div").klass("head").html(_("Selection<br>changes")))
            .add(table().adds(this.mkAll(cos))))
          .add($("td").style("vertical-align: top;")
            .add($("div").klass("head").html(_("Accepted<br>changes")))
            .add(table().adds(this.mkAll(cos))))
          .add($("td").style("vertical-align: top;")
            .add($("div").klass("head").html(_("Rejected<br>changes")))
            .add(table().adds(this.mkAll(cos))))))
    ;
  }

  /**
   * @return {Promise}
   */
  async show () {
    this._main.dom.show(
      Main.volumePageId,
      $("div")
        .add($("h2").style("text-align: center;").html(_("Volume")))
        .add($("table").att("align", "center").klass("frame")
          .add($("tr").add(nickTd)))
        .add(body)
        .add(Ui.upTop("up"))
    );

    const cos = [];
    let nick = "";
    let more = true;
    let cont = true;
    while (more && cont) {
      nickTd.removeAll()
        .add($("div").html(nick))
        .add(Ui.link(() => {
          alert("Stoped");
          cont = false;
        }).klass("link").html("Stop"));
      const data = {
        "source": "volume",
        "rq": "row",
        "nick": nick
      };
      const rp = await this._main.client.send(data);
      more = rp["more"];
      if (more) {
        // row is [type("sel", "in", "out"), nick (string), volume (number)]
        const row = rp["row"];
        nick = row[1];
        cos.push(row);
        this.setBody(cos);
      }
    }
    nickTd.html("---");
  }

}
