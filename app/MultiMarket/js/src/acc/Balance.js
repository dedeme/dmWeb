// Copyright 24-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import AccMain from "./AccMain.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Domo from "../dmjs/Domo.js";  //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";

const $ = e => Ui.$(e);

const balanceSeparator = () => $("td").klass("separator");

const NICK_ORDER = 0;
const VALUE_ORDER = 1;
const PROFITS_ORDER = 2;
const RISK_ORDER = 3;
const BET_ORDER = 4;
const DAYS_ORDER = 5;

function fprofits (stocks, buy, current) {
  return (current - buy) * stocks;
}

function frisk (stocks, buy, ref) {
  return (ref - buy) * stocks;
}

function fbet (stocks, current, ref) {
  return (current - ref) * stocks;
}

function fref (current, ref) {
  return (current - ref) * 1000 / ref;
}

function fcolor (value) {
  if (value < 0) return "aa2800";
  return "0041aa";
}

/** Balance page. */
export default class Balance {

  /**
   * @param {!AccMain} accMain Main
   */
  constructor (accMain) {
    this._accMain = accMain;
    /** @type{string} */
    this._lang = accMain.lang;
    this._currentProfits = 0;
    this._accountProfits = 0;
    this._riskProfits = 0;
    this._bet = 0;

    this._stocks = 0;
    this._cash = 0;
    this._capital = 0;
    this._sells = 0;
    this._fees = 0;
    this._incomes = 0;
    this._differences = 0;

    this._portfolio = [];

    this._order = DAYS_ORDER;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  formatN (n, dec) {
    const d = new Dec(n, dec);
    return n === Infinity ? "∞"
      : n === -Infinity ? "-∞"
        : this._lang === "es" ? d.toIso() : d.toEn();
  }

  sortf () {
    return this._order === NICK_ORDER
      ? (row1, row2) => row1[0] > row2[0] ? 1 : -1
      : this._order === VALUE_ORDER
        ? (row1, row2) => row1[3] * row1[1] > row2[3] * row2[1] ? -1 : 1
        : this._order === PROFITS_ORDER
          ? (row1, row2) =>
            (row1[3] - row1[2]) * row1[1] > (row2[3] - row2[2]) * row2[1]
              ? -1 : 1
          : this._order === RISK_ORDER
            ? (row1, row2) =>
              (row1[4] - row1[2]) * row1[1] > (row2[4] - row2[2]) * row2[1]
                ? -1 : 1
            : this._order === BET_ORDER
              ? (row1, row2) =>
                (row1[3] - row1[4]) * row1[1] > (row2[3] - row2[4]) * row2[1]
                  ? -1 : 1
              : (row1, row2) =>
                fref(row1[3], row1[4]) < fref(row2[3], row2[4])
                  ? -1 : 1
    ;
  }

  body () {
    return $("div")
      .add($("div").klass("head").html(_("Profits")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Current profits") + ":")))
          .add($("td").klass("number")
            .add($("span").html(
              "<font color='0041aa'>" +
              (this._currentProfits === null ? "[?]"
                : this.formatN(this._currentProfits, 2)) +
              "</font>"))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Accounting profits") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._accountProfits, 2)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Risk profits") + ":")))
          .add($("td").klass("number")
            .add($("span").html(
              "<font color='aa2800'>" +
              (this.formatN(this._riskProfits, 2)) +
              "</font>"))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Total bet") + ":")))
          .add($("td").klass("number")
            .add($("span").html(
              "<font color='00aa41'>" +
              (this.formatN(this._bet, 2)) +
              "</font>"))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Reference (‰)") + ":")))
          .add($("td").klass("number")
            .add($("span").html(
              `<font color='${fcolor(this._riskProfits)}'>` +
              (this.formatN(
                this._bet * 1000 /
                  (this._stocks + this._currentProfits - this._accountProfits)
                , 0
              )) +
              "</font>")))))
      .add($("div").klass("head").html(_("Balance")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Assets"))))
          .add(balanceSeparator())
          .add($("td").att("colspan", 2).klass("head")
            .add($("span").html(_("Equity")))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Stocks") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._stocks, 2))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Capital") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._capital, 2)))))
        .add($("tr")
          .add($("td").klass("rlabel")
            .add($("span").html(_("Cash") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._cash, 2))))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Sells") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._sells, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Fees") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._fees, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Profits") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._incomes, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td").klass("rlabel")
            .add($("span").html(_("Differences") + ":")))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(this._differences, 2))))))
      .add($("div").klass("head").html(_("Stocks")))
      .add($("table").att("align", "center").klass("home")
        .add($("tr")
          .add($("td").klass("head")
            .add(Ui.link(() => {
              this._order = NICK_ORDER;
              this.reload();
            }).klass("linkBold").html(_("Co."))))
          .add($("td").klass("head")
            .add($("span").html("Nm.")))
          .add($("td").klass("head")
            .add($("span").html(_("Buy"))))
          .add($("td").klass("head")
            .add($("span").html(_("Sell"))))
          .add($("td").klass("head")
            .add(
              (this._currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = VALUE_ORDER;
                  this.reload();
                }).klass("linkBold")
              ).html(_("Value"))))
          .add($("td").klass("head")
            .add(
              (this._currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = PROFITS_ORDER;
                  this.reload();
                }).klass("linkBold")
              ).html(_("Profits"))))
          .add($("td").klass("head")
            .add(
              (this._currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = RISK_ORDER;
                  this.reload();
                }).klass("linkBold")
              ).html(_("Risk"))))
          .add($("td").klass("head")
            .add(
              (this._currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = BET_ORDER;
                  this.reload();
                }).klass("linkBold")
              ).html(_("Bet"))))
          .add($("td").klass("head")
            .add(
              (this._currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = DAYS_ORDER;
                  this.reload();
                }).klass("linkBold")
              ).html(_("Rf. (‰)")))))
        .adds(this._portfolio.map(e => $("tr")
          .add($("td").klass("nick")
            .add($("span").html(e[0])))
          .add($("td").klass("number2")
            .add($("span").html(this.formatN(e[1], 0))))
          .add($("td").klass("number")
            .add($("span").html(this.formatN(e[2], 4))))
          .add($("td").klass("number")
            .add($("span").html(e[3] === null ? "[?]" : this.formatN(e[3], 4))))
          .add($("td").klass("number")
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN(e[3] * e[1], 2))))
          .add($("td").klass("number") // Profits
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN(fprofits(e[1], e[2], e[3]), 2))))
          .add($("td").klass("number") // Risk
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN(frisk(e[1], e[2], e[4]), 2))))
          .add($("td").klass("number") // Bet
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN(fbet(e[1], e[3], e[4]), 2))))
          .add($("td").klass("number") // Rf
            .add($("span")
              .html(e[3] === null ? "[?]"
                : this.formatN(fref(e[3], e[4]), 0)))))))
      .add(Ui.upTop("up"))
    ;

  }

  reload () {
    this._portfolio.sort(this.sortf());
    this._accMain.view.removeAll().add(this.body());
  }

  /**
   * @return {Promise}
   */
  async show () {
    const data = {
      "module": "acc",
      "source": "Balance",
      "rq": "idata"
    };
    const rp = await Main.client.rq(data);

    const ld = rp["ledger"];
    this._stocks = ld[0];
    this._cash = ld[1];
    this._capital = -ld[2];
    this._sells = -ld[3];
    this._fees = -ld[4];
    this._incomes = -ld[5];
    this._differences = -ld[6];

    this._accountProfits = this._sells + this._fees +
      this._incomes + this._differences;
    this._currentProfits = this._accountProfits;
    this._riskProfits = this._accountProfits;
    this._bet = 0;

    const pf = rp["pf"];
    this._portfolio = pf.map(e => {
      if (e[3] <= 0) {
        e[3] = null;
        this._currentProfits = null;
      } else if (this._currentProfits !== null) {
        this._currentProfits += (e[3] - e[2]) * e[1];
        this._riskProfits += (e[4] - e[2]) * e[1];
        this._bet += (e[3] - e[4]) * e[1];
      }
      return e;
    });

    this.reload();
  }
}

