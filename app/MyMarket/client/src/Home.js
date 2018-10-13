// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

const balanceSeparator = () => $("td").klass("separator");

/** Update page. */
export default class Home {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._currentProfits = 0;
    this._accountableProfits = 0;

    this._stocks = 0;
    this._cash = 0;
    this._capital = 0;
    this._sells = 0;
    this._fees = 0;
    this._incomes = 0;
    this._differences = 0;

    this._inventory = [];
  }

  body () {
    return $("div")
      .add($("div").klass("head").html("Profits"))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Current profits:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._currentProfits)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Accounting profits:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._accountableProfits))))))
      .add($("div").klass("head").html("Balance"))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Assets"))))
          .add(balanceSeparator())
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Equity")))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Stocks:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._stocks))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Capital:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._capital)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Cash:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._cash))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Sells:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._sells)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Fees:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._fees)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Incomes:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._incomes)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Differences:"))))
          .add($("td").klass("number")
            .add($("span").html(String(this._differences))))))
      .add($("div").klass("head").html("Stocks"))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("head")
            .add($("span").html(_("Co."))))
          .add($("td").klass("head")
            .add($("span").html("Nm.")))
          .add($("td").klass("head")
            .add($("span").html(_("Price"))))
          .add($("td").klass("head")
            .add($("span").html(_("Value"))))
          .add($("td").klass("head")
            .add($("span").html(_("Dif.")))))
        .adds(this._inventory.map(e => $("tr")
          .add($("td").klass("nick")
            .add($("span").html(e[0])))
          .add($("td").klass("number2")
            .add($("span").html(String(e[1]))))
          .add($("td").klass("number2")
            .add($("span").html(String(e[2]))))
          .add($("td").klass("number")
            .add($("span").html(String(e[3]))))
          .add($("td").klass("number")
            .add($("span").html(String((e[3] - e[2]) * e[1])))))))
    ;

  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show("home", this.body());
  }
}

