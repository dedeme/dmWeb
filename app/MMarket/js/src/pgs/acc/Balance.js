// Copyright 11-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting balance.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
import {Menu} from "../../dmjs/Menu.js";
import {_} from "../../I18n.js";
import Cts from "../../data/Cts.js";

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

function formatN (n, dec) {
  return n === Infinity ? "∞"
    : n === -Infinity ? "-∞"
      : isNaN(n) ? "[?]" : new Dec(n, dec).toIso();
}

/**
    Accounting balance.
**/
export default class Balance {
  /**
      @param {!Domo} wg
      @param {!Array<!Array<number>>} ledgers There are a ledger for each
        investor. Each ledger Has the following fields:
          0. stocks; 1. cash; 2. capital; 3. sells; 4. fees; 5. incomes;
          6. differences
      @param {!Array<!Array<!Array<?>>>} portfolios There are a portfolio for
        each investor. Each profolio has serveral entries and each entry has
        the following fields:
          0. Nick; 1. stocks; 2. price; 3. quote; 4. ref

  **/
  constructor (wg, ledgers, portfolios) {
    this._wg = wg;
    this._ledgers = ledgers;
    this._portfolios = portfolios;
    this._mSel = -1;

    this._order = DAYS_ORDER;
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return function(number, number):number
  **/
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

  /**
      @param {!Domo} wg
      @param {!Array<number>} ld
      @param {!Array<!Array<?>>} pf
      @return void
  **/
  body (wg, ld, pf) {
    pf.sort(this.sortf());
    const stocks = ld[0];
    const cash = ld[1];
    const capital = -ld[2];
    const sells = -ld[3];
    const fees = -ld[4];
    const incomes = -ld[5];
    const differences = -ld[6];

    const accountProfits = sells + fees + incomes + differences;
    let currentProfits = accountProfits;
    let riskProfits = accountProfits;
    let bet = 0;

    const portfolio = pf.map(e => {
      if (e[3] <= 0) {
        e[3] = null;
        currentProfits = null;
      } else if (currentProfits !== null) {
        currentProfits += (e[3] - e[2]) * e[1];
        riskProfits += (e[4] - e[2]) * e[1];
        bet += (e[3] - e[4]) * e[1];
      }
      return e;
    });

    let sumValues = 0;
    let sumProfits = 0;
    let sumRisks = 0;
    let sumBets = 0;
    if (currentProfits === null) {
      sumValues = null;
      sumProfits = null;
      sumRisks = null;
      sumBets = null;
    } else {
      portfolio.forEach(e => {
        sumValues += e[3] * e[1];
        sumProfits += fprofits(e[1], e[2], e[3]);
        sumRisks += frisk(e[1], e[2], e[4]);
        sumBets += fbet(e[1], e[3], e[4]);
      });
    }

    wg
      .removeAll()
      .add($("div")
        .klass("head")
        .html(_("Profits")))
      .add($("table")
        .att("align", "center")
        .klass("home")
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Current profits") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(
                "<font color='0041aa'>" +
                (currentProfits === null ? "[?]"
                  : formatN(currentProfits, 2)) +
                "</font>"))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Accounting profits") + ":")))
          .add($("td")
            .klass("number")
            .add($("span").html(formatN(accountProfits, 2)))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Risk profits") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(
                "<font color='aa2800'>" +
                formatN(riskProfits, 2) +
                "</font>"))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Total bet") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(
                "<font color='00aa41'>" +
                (formatN(bet, 2)) +
                "</font>"))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Reference (‰)") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(
                `<font color='${fcolor(riskProfits)}'>` +
                formatN(
                  bet * 1000 / (stocks + currentProfits - accountProfits),
                  0
                ) +
                "</font>")))))
      .add($("div")
        .klass("head")
        .html(_("Balance")))
      .add($("table")
        .att("align", "center")
        .klass("home")
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .klass("head")
            .add($("span")
              .html(_("Assets"))))
          .add(balanceSeparator())
          .add($("td")
            .att("colspan", 2)
            .klass("head")
            .add($("span")
              .html(_("Equity")))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Stocks") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(stocks, 2))))
          .add(balanceSeparator())
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Capital") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(capital, 2)))))
        .add($("tr")
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Cash") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(cash, 2))))
          .add(balanceSeparator())
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Sells") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(sells, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Fees") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(fees, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Profits") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(incomes, 2)))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add(balanceSeparator())
          .add($("td")
            .klass("rlabel")
            .add($("span")
              .html(_("Differences") + ":")))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(differences, 2))))))
      .add($("div")
        .klass("head")
        .html(_("Stocks")))
      .add($("table")
        .att("align", "center")
        .klass("home")
        .add($("tr")
          .add($("td")
            .klass("head")
            .add(Ui.link(() => {
              this._order = NICK_ORDER;
              this.body(wg, ld, pf);
            }).klass("linkBold").html(_("Co."))))
          .add($("td").klass("head")
            .add($("span").html("Nm.")))
          .add($("td").klass("head")
            .add($("span").html(_("Buy"))))
          .add($("td").klass("head")
            .add($("span").html(_("Sell"))))
          .add($("td").klass("head")
            .add(
              (currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = VALUE_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Value"))))
          .add($("td")
            .klass("head")
            .add(
              (currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = PROFITS_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Profits"))))
          .add($("td")
            .klass("head")
            .add(
              (currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = RISK_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Risk"))))
          .add($("td")
            .klass("head")
            .add(
              (currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = BET_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Bet"))))
          .add($("td")
            .klass("head")
            .add(
              (currentProfits === null
                ? $("span")
                : Ui.link(() => {
                  this._order = DAYS_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Rf. (‰)")))))
        .adds(portfolio.map(e => $("tr")
          .add($("td")
            .klass("nick")
            .add($("span")
              .html(e[0])))
          .add($("td")
            .klass("number2")
            .add($("span")
              .html(formatN(e[1], 0))))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(formatN(e[2], 4))))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(e[3] === null ? "[?]" : formatN(e[3], 4))))
          .add($("td")
            .klass("number")
            .add($("span")
              .html(e[3] === null ? "[?]" : formatN(e[3] * e[1], 2))))
          .add($("td") // Profits
            .klass("number")
            .add($("span")
              .html(e[3] === null
                ? "[?]"
                : formatN(fprofits(e[1], e[2], e[3]), 2))))
          .add($("td") // Risk
            .klass("number")
            .add($("span")
              .html(e[3] === null
                ? "[?]"
                : formatN(frisk(e[1], e[2], e[4]), 2))))
          .add($("td") // Bet
            .klass("number")
            .add($("span")
              .html(e[3] === null
                ? "[?]"
                : formatN(fbet(e[1], e[3], e[4]), 2))))
          .add($("td")
            .klass("number") // Rf
            .add($("span")
              .html(e[3] === null
                ? "[?]"
                : formatN(fref(e[3], e[4]), 0))))))
        .add($("tr")
          .add($("td"))
          .add($("td"))
          .add($("td"))
          .add($("td"))
          .add($("td")
            .klass("numberSum")
            .add($("span")
              .html(sumValues === null
                ? "[?]"
                : formatN(sumValues, 2))))
          .add($("td")
            .klass("numberSum")
            .add($("span")
              .html(sumProfits === null
                ? "[?]"
                : formatN(sumProfits, 2))))
          .add($("td")
            .klass("numberSum")
            .add($("span")
              .html(sumRisks === null
                ? "[?]"
                : formatN(sumRisks, 2))))
          .add($("td")
            .klass("numberSum")
            .add($("span")
              .html(sumBets === null
                ? "[?]"
                : formatN(sumBets, 2))))
          .add($("td"))
        ))
    ;

  }

  /**
      @private
  **/
  view () {
    const wg = $("div");
    const mSel = this._mSel === -1 ? "All" : _("Inv-") + String(this._mSel);

    const lopts = [
      Menu.toption("All", _("All"), () => this.setMenu(-1))
    ];
    for (let i = 0; i < this._ledgers.length; ++i) {
      const op = _("Inv-") + String(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () => this.setMenu(i)));
    }
    const ropts = [];
    const menu = new Menu(lopts, ropts, mSel);

    let ledger = [0, 0, 0, 0, 0, 0, 0];
    let portfolio = [];
    if (this._mSel === -1) {
      for (let inv = 0; inv < this._ledgers.length; ++inv) {
        const l = this._ledgers[inv];
        for (let i = 0; i < l.length; ++i) {
          ledger[i] += l[i];
        }
        const pf = this._portfolios[inv];
        for (let i = 0; i < pf.length; ++i) {
          const e = pf[i];
          const nick = e[0];
          const i2 = portfolio.findIndex(e2 => e2[0] === nick);
          if (i2 === -1) {
            portfolio.push(e);
          } else {
            const e2 = portfolio[i2];
            const stocks = e[1] + e2[1];
            portfolio[i2] = [
              nick,
              stocks,
              (e[1] * e[2] + e2[1] * e2[2]) / stocks,
              e2[3],
              (e[1] * e[4] + e2[1] * e2[4]) / stocks
            ];
          }
        }
      }
    } else {
      ledger = this._ledgers[this._mSel];
      portfolio = this._portfolios[this._mSel];
    }

    this.body(wg, ledger, portfolio);

    this._wg
      .removeAll()
      .add(menu.wg)
      .add(wg)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {number} manager
      @return void
  **/
  setMenu (manager) {
    this._mSel = manager;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return !Promise<!Profits>
  **/
  static async mk (wg) {
    const rp = await Cts.client.send({
      "module": "acc",
      "source": "balance",
      "rq": "idata"
    });

    const ledgers = rp["ledgers"];
    const portfolios = rp["portfolios"];

    return new Balance(wg, ledgers, portfolios);
  }

}
