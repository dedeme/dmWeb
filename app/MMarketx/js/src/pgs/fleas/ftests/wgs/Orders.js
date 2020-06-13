// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../../../dmjs/Ui.js";
import Dec from "../../../../dmjs/Dec.js";
import {_} from "../../../../I18n.js";
import Broker from "../../../../data/Broker.js";
import Forder from "../../../../data/flea/Forder.js"; //eslint-disable-line
import Cts from "../../../../data/Cts.js";

const $ = e => Ui.$(e);

const NUM = 5;
const CV = 3;
const DATE = 12;
const NICK = 6;
const STOCKS = 9;
const PRICE = 10;
const CASH = 13;

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

function row (num, op, date, nick, stocks, price, cash) {
  return "|" +
    fr(new Dec(num, 0).toIso(), NUM) + ":" +
    fc(op, CV) + ":" +
    fc(date, DATE) + ":" +
    fc(nick, NICK) + ":" +
    fr(new Dec(stocks, 0).toIso(), STOCKS) + ":" +
    fr(new Dec(price, 4).toIso(), PRICE) + ":" +
    fr(new Dec(cash, 2).toIso(), CASH) + "|\n"
  ;
}

/**
    Orders Fleas Tests page.
**/
export default class Operations {
  /**
      @param {!Array<Forder>} orders
      @param {!Array<string>} nicks
      @param {!Array<number>} lastCloses
  **/
  constructor (orders, nicks, lastCloses) {
    this._orders = orders;
    this._nicks = nicks;
    this._lastCloses = lastCloses;

    this._wg = $("TextArea").att("spellcheck", false)
      .att("cols", 68).att("rows", 20);
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
    let cash = Cts.initialCapital;
    const rule = "|" + fff(NUM) + ":" + fff(CV) + ":" + fff(DATE) + ":" +
      fff(NICK) + ":" + fff(STOCKS) + ":" + fff(PRICE) + ":" + fff(CASH) +
      "|\n";

    let tx = rule;
    tx += "|" + fc(_("Nº"), NUM) + ":" + fc("*", CV) + ":" +
      fc(_("Date"), DATE) + ":" + fc(_("Nick"), NICK) + ":" +
      fc(_("Stocks(Operations)"), STOCKS) + ":" + fc(_("Price"), PRICE) + ":" +
      fc(_("Cash"), CASH) + "|\n";
    tx += rule;
    const pf = this._nicks.map(() => 0);
    this._orders.forEach((o, ix) => {
      const op = o.isSell ? _("S(ell)") : _("B(uy)");
      const date = o.date;
      const nick = o.nick;
      const stocks = o.stocks;
      const price = o.price;
      const nickIx = this._nicks.indexOf(nick);
      if (o.isSell) {
        cash += Broker.sell(stocks, price);
        pf[nickIx] = 0;
      } else {
        cash -= Broker.buy(stocks, price);
        pf[nickIx] = stocks;
      }
      tx += row(ix + 1, op, date, nick, stocks, price, cash);
    });
    pf.forEach((e, i) => {
      if (pf[i] > 0)
        cash += Broker.sell(pf[i], this._lastCloses[i]);
    });
    tx += "|" + ff(NUM) + ":" + ff(CV) + ":" + ff(DATE) + ":" +
      ff(NICK) + ":" + ff(STOCKS) + ":" + ff(PRICE) + ":" +
      fr(new Dec(cash, 2).toIso(), CASH) + "|\n";
    tx += rule;

    this._wg.text(tx);
  }
}
