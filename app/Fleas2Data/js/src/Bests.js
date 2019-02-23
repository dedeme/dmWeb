// Copyright 11-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Dec from "./dmjs/Dec.js";

const $ = Ui.$;

/** Bests page. */
export default class Bests {
  /**
   * @param {!Main} main Main
   * @param {string} fgroup Group to show
   */
  constructor (main, fgroup) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /**
     * @private
     * @type {string}
     */
    this._fgroup = fgroup;

    /**
     * @private
     * @type {Array<boolean>}
     */
    this._upDown = [false, false, true, true, true, true, true, true, true];
  }

  calculateDays (genDays) {
    const cs = this._main.constants;
    return parseInt(
      cs["min_days"] + (cs["max_days"] - cs["min_days"]) * genDays, 10
    );
  }

  calculateStrip (genStrip) {
    const cs = this._main.constants;
    return genStrip * cs["max_strip"] * 100;
  }

  showEmpty () {
    this._main.dom.show(Main.bestsPageId, $("div").style("text-align:center")
      .add($("table").att("align", "center")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data")))))
    );
  }

  showData (fgroups, fgroup, results) {
    const main = this._main;

    const mainTdLeft = $("table").klass("frame")
      .adds(fgroups.map(g =>
        $("tr")
          .add($("td").style("text-align:left")
            .add(g === fgroup
              ? $("span").html("<b>" + g + "</b>")
              : Ui.link(() => main.go(Main.bestsPageId, "", g))
                .klass("link").html(g)))))
    ;

    const mainTdRight = $("div");

    const setResults = (rs) => {
      return mainTdRight.removeAll().add(
        $("table").att("align", "center").klass("data")
          .add($("tr")
            .add($("td").klass("header").text(_("Date")))
            .add($("td").klass("header").text(_("Id")))
            .add($("td").klass("header").text(_("Days")))
            .add($("td").klass("header").text(_("Buy Str.")))
            .add($("td").klass("header").text(_("Sell Str.")))
            .add($("td").klass("header").text(_("Assets")))
            .add($("td").klass("header").text(_("Buys")))
            .add($("td").klass("header").text(_("Sells")))
            .add($("td").klass("header").text(_("Average")))
            .add($("td").klass("header").text(_("R.S.D.")))
          )
          .adds(rs.map((r) =>
            $("tr")
              .add($("td").klass("number")
                .text(r[0]))
              .add($("td").klass("menu")
                .text(r[1][0][0]))
              .add($("td").klass("number")
                .text(String(this.calculateDays(Number(r[1][0][1][0])))))
              .add($("td").klass("number")
                .text(new Dec(
                  this.calculateStrip(Number(r[1][0][1][1])), 4
                ).toEu() + "%"))
              .add($("td").klass("number")
                .text(new Dec(
                  this.calculateStrip(Number(r[1][0][1][2])), 4
                ).toEu() + "%"))
              .add($("td").klass("number")
                .text(new Dec(Number(r[1][1]), 2).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[1][2]), 0).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[1][3]), 0).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[2]), 2).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[3] * 100), 4).toEu() + "%"))
          )))
      ;
    };

    const mainTable = $("table").klass("main")
      .add($("tr")
        .add($("td").style("width:5px;whiteSpace:nowrap;vertical-align:top")
          .add(mainTdLeft))
        .add($("td")
          .add(mainTdRight)))
    ;

    this._main.dom.show(Main.bestsPageId, mainTable);

    setResults(results);
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const main = this._main;
    const fgroup = this._fgroup;

    const data = {
      "source": "Bests",
      "rq": "groups",
    };
    const rp = await main.client.send(data);

    const fgrs = rp["fgrs"];
    if (fgrs.length === 0) {
      this.showEmpty();
    } else {
      let fgr = fgrs.find(g => g === fgroup);
      if (fgr === undefined) {
        fgr = fgrs[0];
      }
      const data = {
        "source": "Bests",
        "rq": "results",
        "fgroup": fgr
      };
      const rp = await main.client.send(data);
      const results = rp["results"];
      results.reverse();
      this.showData(fgrs, fgr, results);
    }
  }
}

