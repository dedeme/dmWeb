// Copyright 13-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import {_} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";

const $ = Ui.$;

const bpIn = $("input").att("type", "text");
const bpLa = $("span").klass("frame").html("---");

const spIn = $("input").att("type", "text");
const spLa = $("span").klass("frame").html("---");

const bsIn0 = $("input").att("type", "text").style("width:150px");
const bsCh0 = $("input").att("type", "checkbox");
const bsIn10 = $("input").att("type", "text").style("width:50px");
const bsCh10 = $("input").att("type", "checkbox");
const bsIn11 = $("input").att("type", "text").style("width:50px");
const bsCh11 = $("input").att("type", "checkbox");
const bsLb20 = $("div").klass("disabled").html("&nbsp;");
const bsIn21 = $("input").att("type", "text").style("width:50px");
const bsCh21 = $("input").att("type", "checkbox");
const bsLb31 = $("div").klass("disabled").html("&nbsp;");

const ssIn0 = $("input").att("type", "text").style("width:150px");
const ssCh0 = $("input").att("type", "checkbox");
const ssIn10 = $("input").att("type", "text").style("width:50px");
const ssCh10 = $("input").att("type", "checkbox");
const ssIn11 = $("input").att("type", "text").style("width:50px");
const ssCh11 = $("input").att("type", "checkbox");
const ssLb20 = $("div").klass("disabled").html("&nbsp;");
const ssIn21 = $("input").att("type", "text").style("width:50px");
const ssCh21 = $("input").att("type", "checkbox");
const ssLb31 = $("div").klass("disabled").html("&nbsp;");

const setControlValue = (input) => {
  if (
    isNaN(input.value().trim()) ||
    input.value().trim() === "" ||
    Number(input.value().trim()) < 0
  ) {
    input.value("0");
  }
};

const calc = (type) => {
  let pentry = spIn;
  let plabel = spLa;
  let opEntry = bpIn;
  let opLabel = bpLa;

  let sentry0 = ssIn0;
  let scheck0 = ssCh0;
  let opSEntry0 = bsIn0;
  let opSCheck0 = bsCh0;

  let sentry10 = ssIn10;
  let scheck10 = ssCh10;
  let opSEntry10 = bsIn10;
  let opSCheck10 = bsCh10;
  let sentry11 = ssIn11;
  let scheck11 = ssCh11;
  let opSEntry11 = bsIn11;
  let opSCheck11 = bsCh11;

  let slabel20 = ssLb20;
  let opSLabel20 = bsLb20;
  let sentry21 = ssIn21;
  let scheck21 = ssCh21;
  let opSEntry21 = bsIn21;
  let opSCheck21 = bsCh21;

  let slabel31 = ssLb31;
  let opSLabel31 = bsLb31;

  if (type === "buy") {
    pentry = bpIn;
    plabel = bpLa;
    opEntry = spIn;
    opLabel = spLa;

    sentry0 = bsIn0;
    scheck0 = bsCh0;
    opSEntry0 = ssIn0;
    opSCheck0 = ssCh0;

    sentry10 = bsIn10;
    scheck10 = bsCh10;
    opSEntry10 = ssIn10;
    opSCheck10 = ssCh10;
    sentry11 = bsIn11;
    scheck11 = bsCh11;
    opSEntry11 = ssIn11;
    opSCheck11 = ssCh11;

    slabel20 = bsLb20;
    opSLabel20 = ssLb20;
    sentry21 = bsIn21;
    scheck21 = bsCh21;
    opSEntry21 = ssIn21;
    opSCheck21 = ssCh21;

    slabel31 = bsLb31;
    opSLabel31 = ssLb31;
  }

  const fPrice = () => {
    opEntry.value("");
    opLabel.html("---");
    const v = pentry.value().trim();
    let tx = "---";
    if (v !== "" && Dec.isNumberEu(v)) {
      if (type === "buy") {
        tx = new Dec(Dec.newEu(v, 2).value * 1.01, 2).toEu();
      } else {
        tx = new Dec(Dec.newEu(v, 2).value * 0.99, 2).toEu();
      }
    }
    plabel.removeAll().html(tx);
  };
  pentry.on("input", fPrice);
  Ui.changePoint(pentry);

  const fCheck0 = () => {
    if (scheck0.checked()) {
      setControlValue(sentry0);
      sentry0.disabled(true);

      const val = Number(sentry0.value().trim());
      const val0 = new Dec(val, 0);
      const val10 = new Dec(val0.value / 2, 0);
      const val20 = new Dec(val0.value - val10.value, 0);
      const val11 = new Dec(val0.value / 3, 0);
      const val21 = val11;
      const val31 = new Dec(val0.value - val11.value * 2, 0);
      scheck10.disabled(false);
      sentry10.value(val10.toEu());
      sentry10.disabled(false);
      scheck11.disabled(false);
      sentry11.value(val11.toEu());
      sentry11.disabled(false);

      slabel20.removeAll().klass("disabled").html(val20.toEu());
      scheck21.checked(false);
      scheck21.disabled(true);
      sentry21.value(val21.toEu());
      sentry21.disabled(true);

      slabel31.removeAll().klass("disabled").html(val31.toEu());

      // ----------------------

      opSCheck0.checked(false);
      opSEntry0.value("");
      opSEntry0.disabled(true);

      opSCheck10.checked(false);
      opSCheck10.disabled(true);
      opSEntry10.value("");
      opSEntry10.disabled(true);
      opSCheck11.checked(false);
      opSCheck11.disabled(true);
      opSEntry11.value("");
      opSEntry11.disabled(true);

      opSLabel20.removeAll().klass("disabled").html("&nbsp;");
      opSCheck21.checked(false);
      opSCheck21.disabled(true);
      opSEntry21.value("");
      opSEntry21.disabled(true);

      opSLabel31.removeAll().klass("disabled").html("&nbsp;");
    } else {
      sentry0.disabled(false);

      scheck10.checked(false);
      scheck10.disabled(true);
      sentry10.value("");
      sentry10.disabled(true);
      scheck11.checked(false);
      scheck11.disabled(true);
      sentry11.value("");
      sentry11.disabled(true);

      slabel20.removeAll().klass("disabled").html("&nbsp;");
      scheck21.checked(false);
      scheck21.disabled(true);
      sentry21.value("");
      sentry21.disabled(true);

      slabel31.removeAll().klass("disabled").html("&nbsp;");
    }
  };
  scheck0.on("click", fCheck0);
  Ui.changePoint(sentry0);

  const fCheck10 = () => {
    if (scheck10.checked()) {
      setControlValue(sentry10);
      sentry10.disabled(true);

      const val = Number(sentry0.value().trim());
      const val0 = new Dec(val, 0);
      const val10 = new Dec(val0.value / 2, 0);
      const val20 = new Dec(val0.value - val10.value, 0);
      const val21 = new Dec(val20.value / 2, 0);
      const val31 = new Dec(val0.value - val10.value - val21.value, 0);
      scheck11.disabled(true);
      sentry11.value(sentry10.value());
      sentry11.disabled(true);

      slabel20.removeAll().klass("enabled").html(val20.toEu());
      scheck21.disabled(false);
      sentry21.value(val21.toEu());
      sentry21.disabled(false);

      slabel31.removeAll().klass("disabled").html(val31.toEu());
    } else {
      fCheck0();
    }
  };
  scheck10.on("click", fCheck10);
  Ui.changePoint(sentry10);
  scheck10.disabled(true);
  sentry10.disabled(true);

  const fCheck11 = () => {
    if (scheck11.checked()) {
      setControlValue(sentry11);
      sentry11.disabled(true);

      scheck10.disabled(true);
      sentry10.value(sentry11.value());
      sentry10.disabled(true);

      const val = Number(sentry0.value().trim());
      const val0 = new Dec(val, 0);
      const val11 = new Dec(val0.value / 3, 0);
      const val20 = new Dec(val0.value - val11.value, 0);
      const val21 = val11;
      const val31 = new Dec(val0.value - val11.value * 2, 0);
      scheck10.disabled(true);
      sentry10.value(sentry11.value());
      sentry10.disabled(true);

      slabel20.removeAll().klass("enabled").html(val20.toEu());
      scheck21.disabled(false);
      sentry21.value(val21.toEu());
      sentry21.disabled(false);

      slabel31.removeAll().klass("disabled").html(val31.toEu());
    } else {
      fCheck0();
    }
  };
  scheck11.on("click", fCheck11);
  Ui.changePoint(sentry11);
  scheck11.disabled(true);
  sentry11.disabled(true);

  slabel20.klass("disabled");

  const fCheck21 = () => {
    if (scheck21.checked()) {
      slabel20.klass("disabled");
      sentry21.disabled(true);
      slabel31.klass("enabled");
    } else {
      slabel20.klass("enabled");
      sentry21.disabled(false);
      slabel31.klass("disabled");
    }
  };
  scheck21.on("click", fCheck21);
  Ui.changePoint(sentry21);
  scheck21.disabled(true);
  sentry21.disabled(true);

  slabel31.klass("disabled");

  return $("table").klass(type === "buy" ? "frame4" : "frame3")
    .add($("tr")
      .add($("td").att("colspan", 4)
        .add($("div").klass("head")
          .html(type === "buy" ? _("Buy") : _("Sell")))))
    .add($("tr")
      .add($("td").att("colspan", 4)
        .add($("hr"))))
    .add($("tr")
      .add($("td").att("colspan", 4).style("text-align:center")
        .add($("div")
          .html("<b>" + _("Price") + "</b>"))))
    .add($("tr")
      .add($("td").att("colspan", 4)
        .add(pentry)))
    .add($("tr")
      .add($("td")))
    .add($("tr")
      .add($("td").att("colspan", 4).style("text-align:center")
        .add(plabel)))

    .add($("tr")
      .add($("td").att("colspan", 4)
        .add($("hr"))))
    .add($("tr")
      .add($("td").att("colspan", 4).style("text-align:center")
        .add($("div")
          .html("<b>" + _("Stocks") + "</b>"))))
    .add($("tr")
      .add($("td").add(scheck0))
      .add($("td").att("colspan", 3).add(sentry0)))
    .add($("tr")
      .add($("td").add(scheck10))
      .add($("td").add(sentry10))
      .add($("td").add(scheck11))
      .add($("td").add(sentry11)))
    .add($("tr")
      .add($("td"))
      .add($("td").add(slabel20))
      .add($("td").add(scheck21))
      .add($("td").add(sentry21)))
    .add($("tr")
      .add($("td"))
      .add($("td"))
      .add($("td"))
      .add($("td").add(slabel31)))
  ;
};

const buyCalc = () => calc("buy");

const sellCalc = () => calc("sell");

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

    const paramTd = (start, stop) => $("tr")
      .add($("td").klass("number").html(this.formatN(start * 100, 4) + "%"))
      .add($("td").klass("number").html(this.formatN(stop * 100, 4) + "%"))
    ;

    const table = $("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;vertical-align:top")
          .add(buyCalc()))
        .add($("td").style("width:90%;text-align:center").add(this._body))
        .add($("td").style("text-align:right;vertical-align:top")
          .add(sellCalc())));

    this._main.dom.show(Main.tradingPageId, table);
    this._body.add(Ui.$("img").att("src", "img/wait2.gif"));
    const data = {
      "source": "trading",
      "rq": "idata"
    };
    const rp = await this._main.client.send(data);
    const buys = rp["buys"];
    const sells = rp["sells"];
    const params = rp["params"];

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
          .add($("td").klass("head").html(_("Start")))
          .add($("td").klass("head").html(_("Stop"))))
        .add(paramTd(params[0], params[1])))
    ;
  }
}

