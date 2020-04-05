// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import Maybe from "../dmjs/Maybe.js";
import {_} from "../I18n.js";
import Broker from "../data/Broker.js";
import Defs from "../Defs.js";

const $ = e => Ui.$(e);

const CV = 3;
const DATE = 12;
const STOCKS = 9;
const PRICE = 10;
const BALANCE = 13;
const PROFITS = 12;
const RATIO = 10;

function lastQuote (qs) {
  let ix = qs.length - 1;
  while (ix-- >= 0) {
    if (qs[ix] > 0) return qs[ix];
  }
  return 1;
}

// Format filled.
function ff (width) {
  let r = "";
  while (width-- > 0) r += " ";
  return r;
}

// Format filled with hyphens.
function fff (width) {
  let r = "";
  while (width-- > 0) r += "-";
  return r;
}

function fr (tx, width) {
  const df = width - tx.length;
  return ff(df) + tx;
}

function fc (tx, width) {
  const df = width - tx.length;
  const ldf = (df / 2) | 0;
  let rdf = df - ldf;
  let r = ff(ldf) + tx;
  while (rdf-- > 0) r += " ";
  return r;
}

function row (op, date, stocks, price, balance, profits, ratio) {
  const bal = balance.isJust() ? new Dec(balance.fromJust(), 2).toIso() : "";
  const prof = profits.isJust() ? new Dec(profits.fromJust(), 2).toIso() : "";
  const rat = ratio.isJust() ? new Dec(ratio.fromJust(), 4).toIso() : "";

  return "|" +
    fc(op, CV) + ":" +
    fc(date, DATE) + ":" +
    fr(new Dec(stocks, 0).toIso(), STOCKS) + ":" +
    fr(new Dec(price, 4).toIso(), PRICE) + ":" +
    fr(bal, BALANCE) + ":" +
    fr(prof, PROFITS) + ":" +
    fr(rat, RATIO) + "|\n"
  ;
}

/**
    References Fleas Tests page.
**/
export default class Operations {
  /**
      @param {!Array<string>} dates
      @param {!Array<number>} opens
      @param {!Array<number>} closes
      @param {!Array<number>} refs
  **/
  constructor (dates, opens, closes, refs) {
    this._dates = dates;
    this._opens = opens;
    this._closes = closes;
    this._refs = refs;

    this._wg = $("TextArea").att("spellcheck", false)
      .att("cols", 78).att("rows", 20);
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  view () {
    const ds = this._dates;
    const os = this._opens;
    const cs = this._closes;
    const rs = this._refs;
    const bet = Defs.bet;
    let stocks = 0 | 0;
    let cash = bet;
    let toSell = true;
    let toDo = false;

    const rule = "|" + fff(CV) + ":" + fff(DATE) + ":" + fff(STOCKS) + ":" +
      fff(PRICE) + ":" + fff(BALANCE) + ":" + fff(PROFITS) + ":" +
      fff(RATIO) + "|\n";

    let tx = rule;
    tx += "|" + fc("*", CV) + ":" + fc(_("Date"), DATE) + ":" +
      fc(_("Stocks(Operations)"), STOCKS) + ":" + fc(_("Price"), PRICE) + ":" +
      fc(_("Balance(Operations)"), BALANCE) + ":" + fc(_("Profits"), PROFITS) +
      ":" + fc(_("Ratio"), RATIO) + "|\n";
    tx += rule;

    for (let i = 0; i < ds.length; ++i) {
      const oq = os[i];
      if (toDo && oq > 0) {
        if (toSell) { // there is buy order.
          stocks = (cash / oq) | 0;
          cash -= Broker.buy(stocks, oq);

          const date = ds[i];
          tx += row(
            _("B(uy)"), date, stocks, oq,
            Maybe.nothing, Maybe.nothing, Maybe.nothing
          );
        } else if (stocks > 0) {
          cash += Broker.sell(stocks, oq);

          const date = ds[i];
          const profits = cash - bet;
          tx += row(
            _("S(ell)"), date, stocks, oq,
            Maybe.just(cash),
            Maybe.just(profits),
            Maybe.just(profits / bet)
          );

          stocks = 0;
        }
        toDo = false;
      }

      const q = cs[i];

      if (q < 0) continue;

      const ref = rs[i];
      if (toSell) {
        if (ref > q) {
          toDo = true;
          toSell = false;
        }
      } else if (ref < q) {
        toDo = true;
        toSell = true;
      }
    }

    if (stocks > 0) {
      const q = lastQuote(cs);
      cash += Broker.sell(stocks, q);
      const date = ds[ds.length - 1];
      const profits = cash - bet;
      tx += row(
        "V", date, stocks, q,
        Maybe.just(cash),
        Maybe.just(profits),
        Maybe.just(profits / bet)
      );
    }

    tx += rule;

    this._wg.text(tx);
  }

}
