// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Main from "./Main.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

/** Index page. */
export default class Index {
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

  /**
   * @private
   * @param {!Array<!Array<string>>} entries
   * @return {void}
   */
  show2 (entries) {
    const main = this._main;
    const sel = main.model.sel;

    const pg = $("table").klass("frame").att("width", "100%")
      .adds(
        entries.map(e =>
          $("tr")
            .add($("td").style("width:5px;white-space:nowrap")
              .add(e[1] === ""
                ? $("span").html(`<b>${e[0]}</b>`)
                : $("span")
                  .klass("link")
                  .html(`<a href="?${sel}@${e[1]}">${e[0]}</a>`
                  )
              ))
            .add($("td").style("width: 5px"))
            .add($("td").html(e[2]))
        )
      );

    main.dom.show(pg);
    $("@title").text(sel);
  }

  /** @return {void} */
  show () {
    const self = this;
    const main = self._main;
    const sel = main.model.sel;
    const path = main.model.paths.find(p => p.id === sel);
    if (path === undefined) {
      main.go("@");
      return;
    }
    const rq = {
      "page": "index",
      "path": path.path
    };
    main.client.send(rq, rp => {
      const entries = rp["entries"];
      if (entries === undefined) {
        main.go("@");
        return;
      }
      self.show2(entries);
    });
  }
}
