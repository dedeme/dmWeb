// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

/** Create page. */
export default class Annotations {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._editor = $("div");
    this._balance = $("span");
    this._historic = $("div");
  }

  setData (balance, annotations) {
    this._balance.removeAll()
      .html(String(balance));

    this._historic.removeAll()
      .add($("table")
        .adds(annotations.map(ann =>
          $("tr")
            .add($("td")
              .add(Ui.link(() => {
                alert("delete " + String(ann[0]));
              }).add(Ui.img("delete"))))
            .add($("td")
              .add(Ui.link(() => {
                alert("modify " + String(ann[0]));
              }).add(Ui.img("edit"))))
            .add($("td")
              .add($("span").html(ann[1])))
            .add($("td")
              .add($("span").html(String(ann[2])))))))
    ;
  }

  body () {
    return $("div")
      .add($("div").klass("head").html(_("Annotations")))
      .add($("table").att("align", "center").klass("frame3")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => {
              alert("new");
            }).klass("link").html(_("new")))))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add(this._editor)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add($("table")
              .add($("tr")
                .add($("td").klass("rlabel")
                  .add($("span").html(_("Balance:"))))
                .add($("td").klass("number")
                  .add(this._balance))
                .add($("td"))))))
        .add($("tr")
          .add($("td").klass("frame")
            .add(this._historic)))
      )
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show("annotations", this.body());
    this.setData(125000, []);
  }
}

