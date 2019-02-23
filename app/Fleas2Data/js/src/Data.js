// Copyright 31-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Dec from "./dmjs/Dec.js";
import DateDm from "./dmjs/DateDm.js";

const $ = Ui.$;

/** Data page. */
export default class Data {
  /**
   * @param {!Main} main Main
   * @param {string} fmodel Flea models
   * @param {string} date Date to show (it can be "")
   */
  constructor (main, fmodel, date) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /**
     * @private
     * @type {string}
     */
    this._fmodel = fmodel;

    /**
     * @private
     * @type {string}
     */
    this._date = date;

    /**
     * @private
     * @type {Array<string>}
     */
    this._paramNames = null;

    /**
     * @private
     * @type {Array<string>}
     */
    this._paramFormats = null;

    /**
     * @private
     * @type {Array<boolean>}
     */
    this._upDown = null;
  }

  resetUpDown () {
    this._upDown = [true, false];
    It.range(this._paramNames.length + 4).each(() => {
      this._upDown.push(true);
    });
  }

  showEmpty (fmodel) {
    this._main.dom.show(fmodel, $("div").style("text-align:center")
      .add($("table").att("align", "center")
        .add($("tr").add($("td").klass("frame").html(
          _("Without data")))))
    );
  }

  showData (fmodel, dates, date, results) {
    const main = this._main;

    const paramsN = this._paramNames.length;
    const assetsCol = paramsN + 2;
    const profitsCol = assetsCol + 1;
    const buysCol = profitsCol + 1;
    const sellsCol = buysCol + 1;

    const assetsSort = (r1, r2) => {
      if (this._upDown[assetsCol]) {
        [r1, r2] = [r2, r1];
      }
      return (r1[3] - r2[3]);
    };

    const dateSort = (r1, r2) => {
      let [d1, d2] = [r1[0], r2[0]];
      if (this._upDown[sellsCol]) {
        [d1, d2] = [d2, d1];
      }

      return d1.toString() === d2.toString()
        ? assetsSort(r1, r2)
        : d1 - d2
      ;
    };

    const idSort = (r1, r2) => {
      function fsort (id1, id2) {
        const s1 = id1.split("-");
        const s2 = id2.split("-");
        if (s1[0] === s2[0]) {
          if (s1[1] === s2[1]) {
            return assetsSort(r1, r2);
          }
          return Number(s1[1]) > Number(s2[1]) ? 1 : -1;
        }
        return Number(s1[0]) > Number(s2[0]) ? 1 : -1;
      }

      let [s1, s2] = [r1[1][0], r2[1][0]];
      if (this._upDown[0]) {
        [s1, s2] = [s2, s1];
      }

      return s1 === s2
        ? assetsSort(r1, r2)
        : fsort(s1, s2)
      ;
    };

    const paramSort = (ix, r1, r2) => {
      let [n1, n2] = [r1[2][ix], r2[2][ix]];
      if (this._upDown[ix + 1]) {
        [n1, n2] = [n2, n1];
      }

      return n1 === n2
        ? assetsSort(r1, r2)
        : n1 - n2
      ;
    };

    const nProfitsSort = (r1, r2) => {
      if (this._upDown[profitsCol]) {
        [r1, r2] = [r2, r1];
      }
      return (r1[4] - r2[4]);
    };

    const nBuysSort = (r1, r2) => {
      let [n1, n2] = [r1[5], r2[5]];
      if (this._upDown[buysCol]) {
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
      let [n1, n2] = [r1[6], r2[6]];
      if (this._upDown[sellsCol]) {
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
              : Ui.link(() => main.go(fmodel, d, "")).klass("link").html(d)))))
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
                this._upDown[assetsCol] = true;
                results.sort(dateSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[0] = !v;
                setResults(results);
              }).klass("link").text(_("Date"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[1];
                this._upDown[assetsCol] = true;
                results.sort(idSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[1] = !v;
                setResults(results);
              }).klass("link").text(_("Id"))))
            .adds(this._paramNames.map((nm, ix) =>
              $("td").klass("header")
                .add(Ui.link(() => {
                  const v = this._upDown[2 + ix];
                  this._upDown[assetsCol] = true;
                  results.sort((r1, r2) => paramSort(ix, r1, r2));
                  setResults(results);
                  this.resetUpDown();
                  this._upDown[2 + ix] = !v;
                  setResults(results);
                }).klass("link").text(nm))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[assetsCol];
                results.sort(assetsSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[assetsCol] = !v;
              }).klass("link").text(_("Assets"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[profitsCol];
                this._upDown[assetsCol] = true;
                results.sort(nProfitsSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[profitsCol] = !v;
              }).klass("link").text(_("Profits"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[buysCol];
                this._upDown[assetsCol] = true;
                results.sort(nBuysSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[buysCol] = !v;
                setResults(results);
              }).klass("link").text(_("Buys"))))
            .add($("td").klass("header")
              .add(Ui.link(() => {
                const v = this._upDown[sellsCol];
                this._upDown[assetsCol] = true;
                results.sort(nSellsSort);
                setResults(results);
                this.resetUpDown();
                this._upDown[sellsCol] = !v;
                setResults(results);
              }).klass("link").text(_("Sells"))))
          )
          .adds(rs.map((r, i) =>
            $("tr")
              .add($("td").klass("number")
                .text(String(i + 1)))
              .add($("td").klass("menu")
                .text(DateDm.fromStr(r[0]).toString()))
              .add($("td").klass("menu")
                .text(r[1][0]))
              .adds(this._paramFormats.map((fm, ix) =>
                $("td").klass("number")
                  .text(fm[0] +
                    new Dec(r[2][ix] * fm[1], fm[2]).toEu() + fm[3])))
              .add($("td").klass("number")
                .text(new Dec(r[3], 2).toEu()))
              .add($("td").klass("number")
                .text(new Dec(r[4] * 100, 2).toEu() + "%"))
              .add($("td").klass("number")
                .text(new Dec(r[5], 0).toEu()))
              .add($("td").klass("number")
                .text(new Dec(r[6], 0).toEu()))
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

    this._main.dom.show(fmodel, $("div")
      .add(mainTable)
      .add(Ui.upTop("up"))
    );

    results.sort(assetsSort);
    setResults(results);
    this._upDown[assetsCol] = !this._upDown[assetsCol];
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const main = this._main;
    const fmodel = this._fmodel;
    let date = this._date;

    const data = {
      "source": "Data",
      "rq": "idata",
      "fmodel": fmodel
    };
    const rp = await main.client.send(data);
    this._paramNames = rp["conf"][0];
    this._paramFormats = rp["conf"][1];
    this._upDown = [true, false];
    It.range(this._paramNames.length + 4).each(() => {
      this._upDown.push(true);
    });

    const dates = rp["dates"];
    if (dates.length === 0) {
      this.showEmpty(fmodel);
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
          "fmodel": fmodel,
          "date": date,
          "chunk": c
        };
        const rp = await main.client.send(data);
        const results = rp["results"];
        if (results.length === 0) {
          this.showData(fmodel, dates, date, allResults);
          break;
        } else {
          allResults = allResults.concat(results);
          ++c;
        }
      }

    }
  }
}

