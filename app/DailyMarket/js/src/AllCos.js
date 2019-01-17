// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";

const $ = Ui.$;

/** Update page. */
export default class AllCos {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._chs = [...It.range(main.data.cos.size)]
      .map(() => $("div").html("here"));
  }

  orderDiv () {
    const self = this;
    const link = (id, f) => Ui.link(() => f()).klass("link").html(id);
    return $("div")
      .add($("span").html(_("Order by: ")))
      .add(link(_("Nick"), self.nickOrder))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(_("Profits"), self.profitOrder))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(_("Risk"), self.riskOrder))
      .add($("span").html("&nbsp;&nbsp;&nbsp;"))
      .add(link(_("Signal"), self.signalOrder))
      .add($("span").html("&nbsp;&nbsp;|&nbsp;"))
      .add(link(_("Reverse"), self.reverseOrder))
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
      tr.add($("td").add(this._chs[c]));
      c++;
    }
    trs.push(tr);
    return $("table").klass("home").att("align", "center").adds(trs);
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show(Main.allCosPageId, $("div").style("text-align:center;")
      .add(this.orderDiv())
      .add(this.chartsTable())
    );
  }
}

