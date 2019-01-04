// Copyright 13-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";

const $ = Ui.$;

/** Update page. */
export default class Trading {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /**
     * @private
     */
    this._body = $("div").style("text-align:center");
  }

  formatN (n, dec) {
    const d = new Dec(n, dec);
    return this._main.model["lang"] === "es" ? d.toEu() : d.toEn();
  }

  /**
   * @return {Promise}
   */
  async show () {
    const addTds = rows => {
      if (rows.length === 0) {
        return [$("tr")
          .add($("td").att("colspan", 2).klass("border")
            .html(_("Without operations")))];
      }
      return rows.map(r => $("tr")
        .add($("td").klass("nick").html(r[0]))
        .add($("td").klass("number").html(this.formatN(r[1], 0)))
      );
    };

    const paramTd = (tx, d, b, s) => $("tr")
      .add($("td").klass("nick").html(tx))
      .add($("td").klass("number").html(this.formatN(d, 0)))
      .add($("td").klass("number").html(this.formatN(b * 100, 4) + "%"))
      .add($("td").klass("number").html(this.formatN(s * 100, 4) + "%"))
    ;

    const paramTds = data => data.map(
      row => paramTd(row[0], row[1][0], row[1][1], row[1][2])
    );

    this._main.dom.show(Main.tradingPageId, this._body);
    this._body.add(Ui.$("img").att("src", "img/wait2.gif"));
    const data = {
      "source": "trading",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);
    const buys = rp["buys"];
    const sells = rp["sells"];
    const params = rp["params"];
    const nkps = rp["nkps"];
    nkps.sort((nkp1, nkp2) => nkp1[0] > nkp2[0] ? 1 : -1);

    this._body.removeAll()
      .add($("div").klass("head").html(_("Buys")))
      .add($("table").att("align", "center").klass("buys")
        .add($("tr")
          .add($("td").klass("head").html(_("Co.")))
          .add($("td").klass("head").html(_("Stocks"))))
        .adds(addTds(buys)))
      .add($("div").klass("head").html(_("Sells")))
      .add($("table").att("align", "center").klass("sells")
        .add($("tr")
          .add($("td").klass("head").html(_("Co.")))
          .add($("td").klass("head").html(_("Stocks"))))
        .adds(addTds(sells)))
      .add($("div").klass("head").html(_("Parameters")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("head").html(_("Co.")))
          .add($("td").klass("head").html(_("Days")))
          .add($("td").klass("head").html(_("Buy")))
          .add($("td").klass("head").html(_("Sell"))))
        .add($("tr")
          .add($("td").att("colspan", 4).klass("border")
            .html("<i>" + _("Buys") + "</i>")))
        .add(paramTd("", params[0], params[1], params[2]))
        .add($("tr")
          .add($("td").att("colspan", 4).klass("border").add($("hr"))))
        .add($("tr")
          .add($("td").att("colspan", 4).klass("border")
            .html("<i>" + _("Sells") + "</i>")))
        .adds(paramTds(nkps))
      )
    ;
  }
}
