// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Code page */
// eslint-disable-next-line
import Main from "./Main.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";

const $ = Ui.$;

/** Code page. */
export default class Code {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /** @private */
  mkFoot () {
    return $("div")
      .adds([...It.range(35).map(() => $("div").html("&nbsp"))])
      .add($("div").style("position: fixed;bottom: 0px;right: 20px")
        .add(Ui.link(() => location.assign("#"))
          .add(Ui.img("up"))))
    ;
  }

  /** @private */
  show2 (html) {
    html = html.trim();
    const main = this._main;
    const model = main.model;
    const module = model.module;
    const link = `?${model.sel}@${module}`;
    const numbers =
      [...It.range(html.split("\n").length).map(i => String(i + 1))].join("\n");
    const pg = $("div")
      .add($("table").klass("frame")
        .add($("tr")
          .add($("td").html(`<a href="${link}">${module}`))))
      .add($("br"))
      .add($("table").style("width:100%;").att("cellspacing", 0)
        .add($("tr")
          .add($("td").klass("prel")
            .add($("div")
              .add($("pre").style("text-align:right;").html(numbers))))
          .add($("td").klass("prer")
            .add($("div")
              .add($("pre").html(html))))))
      .add(this.mkFoot())
    ;
    main.dom.show(pg);

    if (main.model.link !== "hp:") {
      document.getElementById(main.model.link).scrollIntoView();
    }
  }

  /**
   * @return {Promise}
   */
  async show () {
    const self = this;
    const main = self._main;
    const sel = main.model.sel;
    const module = main.model.module;
    const path = main.model.paths.find(p => p.id === sel);
    if (path === undefined || module === "") {
      main.go("@");
      return;
    }

    const rq = {
      "page": "code",
      "path": path.path + "/" + module + ".js",
      "link": main.model.link
    };
    const rp = await main.client.send(rq);
    const html = rp["html"];
    if (html === undefined) {
      main.go("@");
      return;
    }
    self.show2(html);
  }
}
