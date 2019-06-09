// Copyright 03-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../dmjs/Domo.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
const $ = Ui.$;

function blanks (tx, width) {
  while (tx.length < width) {
    tx = " " + tx;
  }
  return tx;
}

function fm (n, dec, width) {
  return blanks(new Dec(n, dec).toEu(), width);
}

/** Company char. */
export default class Operations {
  /**
   * @param {!Array<?>} ops: Operations
   */
  constructor (ops) {
    /** @private **/
    this._ops = ops;
  }

  /** return {!Domo} **/
  wg () {
    const bet = 15000;
    let tx = "";
    let price = 0;
    let profs = 0;
    let tt = 0;
    let perc = 0;
    this._ops.forEach(o => {
      if (o[0]) {
        profs = o[1] * (o[2] - price);
        tt += profs;
        perc = tt * 100 / bet;
      } else {
        price = o[2];
      }
      if (!o[0] || o[1] > 0) {
        tx += "| " +
          (o[0] ? "V" : "C") + " : " +
          fm(o[1], 0, 7) + " : " +
          fm(o[2], 4, 8) + " : " +
          (o[0] ? fm(profs, 2, 10) : blanks("", 10)) + " : " +
          (o[0] ? fm(tt, 2, 11) : blanks("", 11)) + " : " +
          (o[0] ? (fm(perc, 2, 7) + "%") : blanks("", 8)) +
          " |\n"
        ;
      }
    });
    return $("TextArea").att("cols", 78).att("rows", 20).value(tx);
  }

}