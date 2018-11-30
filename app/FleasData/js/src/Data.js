// Copyright 31-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Dec from "./dmjs/Dec.js";

const $ = Ui.$;

/** Data page. */
export default class Data {
  /**
   * @param {!Main} main Main
   * @param {string} fgroup Fleas group
   * @param {string} date Date to show (it can be "")
   */
  constructor (main, fgroup, date) {
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
     * @type {string}
     */
    this._date = date;

    /**
     * @private
     * @type {Array<boolean>}
     */
    this._upDown = [false, true, true, true, true, true, true, true];
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

  resetUpDown () {
    this._upDown = [false, true, true, true, true, true, true, true];
  }

  showEmpty (fgroup) {
    this._main.dom.show(fgroup, $("div").style("text-align:center")
      .add($("table").att("align", "center")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data")))))
    );
  }

  showData (fgroup, dates, date, results) {
    const main = this._main;

    const assetsSort = (r1, r2) => {
      if (this._upDown[4]) {
        [r1, r2] = [r2, r1];
      }
      return (Number(r1[1]) - Number(r2[1]));
    };

    const idSort = (r1, r2) => {
      let [s1, s2] = [r1[0][0], r2[0][0]];
      if (this._upDown[0]) {
        [s1, s2] = [s2, s1];
      }

      return s1 === s2
        ? assetsSort(r1, r2)
        : Number(s1.replace("-", ".")) - Number(s2.replace("-", "."))
      ;
    };

    const daysSort = (r1, r2) => {
      let [n1, n2] = [
        this.calculateDays(Number(r1[0][1][0])),
        this.calculateDays(Number(r2[0][1][0]))
      ];
      if (this._upDown[1]) {
        [n1, n2] = [n2, n1];
      }

      return n1 === n2
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const buyStSort = (r1, r2) => {
      let [n1, n2] = [
        this.calculateStrip(Number(r1[0][1][1])),
        this.calculateStrip(Number(r2[0][1][1]))
      ];
      if (this._upDown[2]) {
        [n1, n2] = [n2, n1];
      }

      const d1 = new Dec(n1, 2);
      const d2 = new Dec(n2, 2);
      return d1.toString() === d2.toString()
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const sellStSort = (r1, r2) => {
      let [n1, n2] = [
        this.calculateStrip(Number(r1[0][1][2])),
        this.calculateStrip(Number(r2[0][1][2]))
      ];
      if (this._upDown[3]) {
        [n1, n2] = [n2, n1];
      }

      const d1 = new Dec(n1, 2);
      const d2 = new Dec(n2, 2);
      return d1.toString() === d2.toString()
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const nBuysSort = (r1, r2) => {
      let [n1, n2] = [Number(r1[3]), Number(r2[3])];
      if (this._upDown[6]) {
        [n1, n2] = [n2, n1];
      }

      const d1 = new Dec(n1, 0);
      const d2 = new Dec(n2, 0);
      return d1.toString() === d2.toString()
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const nSellsSort = (r1, r2) => {
      let [n1, n2] = [Number(r1[4]), Number(r2[4])];
      if (this._upDown[7]) {
        [n1, n2] = [n2, n1];
      }

      const d1 = new Dec(n1, 0);
      const d2 = new Dec(n2, 0);
      return d1.toString() === d2.toString()
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const mainTdLeft = $("table").klass("frame")
      .adds(dates.map(d =>
        $("tr")
          .add($("td").style("text-align:left")
            .add(d === date
              ? $("span").html("<b>" + d + "</b>")
              : Ui.link(() => main.go(fgroup, d)).klass("link").html(d)))))
    ;

    const mainTdRight = $("div");

    const setResults = (rs) => {
      return mainTdRight.removeAll().add(
        $("table").att("align", "center").klass("data")
          .add($("tr")
            .add($("td").klass("header").text(_("Nº")))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[0];
                this._upDown[4] = true;
                results.sort(idSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[0] = !v;
                setResults(results);
              }).klass("link").text(_("Id"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[1];
                this._upDown[4] = true;
                results.sort(daysSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[1] = !v;
                setResults(results);
              }).klass("link").text(_("Days"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[2];
                this._upDown[4] = true;
                results.sort(buyStSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[2] = !v;
                setResults(results);
              }).klass("link").text(_("Buy Str."))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[3];
                this._upDown[4] = true;
                results.sort(sellStSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[3] = !v;
                setResults(results);
              }).klass("link").text(_("Sell Str."))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[4];
                results.sort(assetsSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[4] = !v;
              }).klass("link").text(_("Assets"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[6];
                this._upDown[4] = true;
                results.sort(nBuysSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[6] = !v;
                setResults(results);
              }).klass("link").text(_("Buys"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[7];
                this._upDown[4] = true;
                results.sort(nSellsSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[7] = !v;
                setResults(results);
              }).klass("link").text(_("Sells"))))
          )
          .adds(rs.map((r, i) =>
            $("tr")
              .add($("td").klass("number")
                .text(String(i + 1)))
              .add($("td").klass("menu")
                .text(r[0][0]))
              .add($("td").klass("number")
                .text(String(this.calculateDays(Number(r[0][1][0])))))
              .add($("td").klass("number")
                .text(new Dec(
                  this.calculateStrip(Number(r[0][1][1])), 2
                ).toEu() + "%"))
              .add($("td").klass("number")
                .text(new Dec(
                  this.calculateStrip(Number(r[0][1][2])), 2
                ).toEu() + "%"))
              .add($("td").klass("number")
                .text(new Dec(Number(r[1]), 2).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[2]), 0).toEu()))
              .add($("td").klass("number")
                .text(new Dec(Number(r[3]), 0).toEu()))
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

    this._main.dom.show(fgroup, mainTable);

    results.sort(assetsSort);
    setResults(results);
    this._upDown[4] = !this._upDown[4];
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const main = this._main;
    const fgroup = this._fgroup;
    let date = this._date;

    const data = {
      "source": "Data",
      "rq": "dates",
      "fgroup": fgroup
    };
    const rp = await main.client.send(data);

    const dates = rp["dates"];
    if (dates.length === 0) {
      this.showEmpty(fgroup);
    } else {
      if (dates.indexOf(date) === -1) {
        date = dates[0];
      }
      let c = 0;
      let allResults = [];
      while (true) {
        const data = {
          "source": "Data",
          "rq": "results",
          "fgroup": fgroup,
          "date": date,
          "chunk": c
        };
        const rp = await main.client.send(data);
        const results = rp["results"];
        if (results.length === 0) {
          this.showData(fgroup, dates, date, allResults);
          break;
        } else {
          allResults = allResults.concat(results);
          ++c;
        }
      }

    }
  }
}

