// Copyright 17-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import ChSmall from "./ChSmall.js";

const $ = Ui.$;


const ORDER_DAY = 0;
const ORDER_NICK = 1;
const ORDER_PROFITS = 2;
const ORDER_RISK = 3;
const ORDER_BET = 4;
const ORDER_SIGNAL = 5;

const orderDiv = $("div").style("padding-bottom:8px");

/** Update page. */
export default class Cos {
  /**
   * @param {!Main} main Main
   * @param {number} type 0 -> all, 1 -> portfolio, 2 -> selected
   */
  constructor (main, type) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    /** @private */
    this._type = type;

    const mcos = this._main.data.cos;
    let cos = [...mcos.keys()].map(k => [k, mcos.get(k)]);
    if (type === 1) {
      cos = cos.filter(nkCo => nkCo[1].stocks > 0);
    } else if (type === 2) {
      cos = cos.filter(nkCo => main.isSel(nkCo[0]));
    }

    /** @private */
    this._chs = [...It.range(cos.length)]
      .map(() => new ChSmall(main));

    /** @private */
    this._order = ORDER_DAY;

    /** @private */
    this._reverse = false;
  }

  showData () {
    this.setOrderDiv();

    const mcos = this._main.data.cos;
    let cos = [...mcos.keys()].map(k => [k, mcos.get(k)]);
    if (this._type === 1) {
      cos = cos.filter(nkCo => nkCo[1].stocks > 0);
    } else if (this._type === 2) {
      cos = cos.filter(nkCo => this._main.isSel(nkCo[0]));
    }

    const order = this._order;
    if (order === ORDER_DAY) {
      cos.sort((e1, e2) => e2[1].dayProfits === e1[1].dayProfits
        ? e2[1].dayRatio - e1[1].dayRatio
        : e2[1].dayProfits - e1[1].dayProfits);
    } else if (order === ORDER_NICK) {
      cos.sort((e1, e2) => e1[0] > e2[0] ? 1 : -1);
    } else if (order === ORDER_PROFITS) {
      cos.sort((e1, e2) => e2[1].profits - e1[1].profits);
    } else if (order === ORDER_RISK) {
      cos.sort((e1, e2) => e1[1].risk - e2[1].risk);
    } else if (order === ORDER_BET) {
      cos.sort((e1, e2) =>
        (e2[1].profits - e2[1].risk) - (e1[1].profits - e1[1].risk)
      );
    } else {
      cos.sort((e1, e2) => e2[1].signalRatio - e1[1].signalRatio);
    }

    if (this._reverse) {
      cos.reverse();
    }

    cos.forEach((e, i) => {
      this._chs[i].update(e[0], e[1]);
    });
  }

  nickOrder () {
    this._order = ORDER_NICK;
    this._reverse = false;
    this.showData();
  }

  profitsOrder () {
    this._order = ORDER_PROFITS;
    this._reverse = false;
    this.showData();
  }

  betOrder () {
    this._order = ORDER_BET;
    this._reverse = false;
    this.showData();
  }

  riskOrder () {
    this._order = ORDER_RISK;
    this._reverse = false;
    this.showData();
  }

  signalOrder () {
    this._order = ORDER_SIGNAL;
    this._reverse = false;
    this.showData();
  }

  dayOrder () {
    this._order = ORDER_DAY;
    this._reverse = false;
    this.showData();
  }

  reverseOrder () {
    this._reverse = !this._reverse;
    this.showData();
  }

  setOrderDiv () {
    const o = this._order;
    const link = (order, id, f) => {
      if (o === order) {
        return Ui.link(f)
          .klass("link").klass("frame").html(id);
      }
      return Ui.link(f).klass("link").html(id);
    };
    orderDiv.removeAll()
      .add($("span").html(_("Order by") + ":&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_DAY, _("Day"), this.dayOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_NICK, _("Nick"), this.nickOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_PROFITS, _("Profits"), this.profitsOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_RISK, _("Risk"), this.riskOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_BET, _("Bet"), this.betOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(ORDER_SIGNAL, _("Signal"), this.signalOrder.bind(this)))
      .add($("span").html("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"))
      .add(link(-1, _("Reverse"), this.reverseOrder.bind(this)))
    ;
  }

  chartsTable () {
    const trs = [];
    let c = 0;
    let tr = $("tr");
    while (c < this._chs.length) {
      if (c % 3 === 0) {
        if (c > 0) trs.push(tr);
        tr = $("tr");
      }
      tr.add($("td").add(this._chs[c].wg()));
      c++;
    }
    trs.push(tr);
    return $("table").klass("frame").att("align", "center").adds(trs);
  }

  /**
   * @return {void}
   */
  show () {
    const page = this._type === 0 ? Main.allCosPageId
      : this._type === 1 ? Main.portfolioPageId
        : Main.selectionPageId
    ;
    this._main.dom.show(page, $("div").style("text-align:center;")
      .add(orderDiv)
      .add(this.chartsTable())
      .add(Ui.upTop("up"))
    );
    if (this._type === 0) {
      this.signalOrder();
    } else if (this._type === 1) {
      this.betOrder();
    } else {
      this.dayOrder();
    }

    this.setOrderDiv();
  }
}

