// Copyright 18-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Annotations widgets.
**/

import Maybe from "../../../../dmjs/Maybe.js"; //eslint-disable-line
import DateDm from "../../../../dmjs/DateDm.js";
import Ui from "../../../../dmjs/Ui.js";
import Domo from "../../../../dmjs/Domo.js";  //eslint-disable-line
import Dec from "../../../../dmjs/Dec.js";
import {_} from "../../../../I18n.js";

const $ = e => Ui.$(e);

export default class Annotations {
  /**
    @param {!Domo} wg
    @param {!Array<?>} anns Annotations. Each annotations is an Array:
            [0 - number] -> identifier
            [1 - date] -> date in format YYYYMMDD
            [2 - string] -> Type. One of se - bu - st - in - wi - pr - fe -
                                         pd - nd.
            ... -> More fields depending on 'Type'.
    @param {!Maybe<function(number)>} del
  **/
  constructor (wg, anns, del) {
    this._wg = wg;
    this._anns = anns;
    this._del = del;

    this.view();
  }

  // View ----------------------------------------------------------------------



  /**
    @private
    @return void
  **/
  view () {
    const fDate = d => DateDm.fromStr(d).format("%D/%M/%Y");
    const fNumber = (n, d) => new Dec(n, d).toIso();

    const tdDate = d => $("td").klass("border").html(fDate(d));
    const tdTp = t => $("td").klass("border").html(t);
    const tdRest = s => $("td").klass("border")
      .style("text-align: left;").html(s);

    const addSeBu = ann => [
      tdDate(ann[1]),
      ann[2] === "se" ? tdTp(_("Sell"))
        : ann[2] === "bu" ? tdTp(_("Buy"))
          : tdTp(_("In Stock")),
      tdRest(`${ann[3]} | ${fNumber(ann[4], 0)} | ${fNumber(ann[5], 4)}`)];
    const addInWi = ann => [
      tdDate(ann[1]),
      ann[2] === "in" ? tdTp(_("Income")) : tdTp(_("Withdrawal")),
      tdRest(`${fNumber(ann[3], 2)}`)];
    const addPrFePdNd = ann => [
      tdDate(ann[1]),
      ann[2] === "pr" ? tdTp(_("Profits"))
        : ann[2] === "fe" ? tdTp(_("Fees"))
          : ann[2] === "pd" ? tdTp(_("Diff. +"))
            : tdTp(_("Diff. -")),
      tdRest(`${fNumber(ann[3], 2)} | ${ann[4]}`)];

    const addAnn = ann =>
      ann[2] === "se" || ann[2] === "bu" || ann[2] === "st" ? addSeBu(ann)
        : ann[2] === "in" || ann[2] === "wi" ? addInWi(ann)
          : addPrFePdNd(ann)
    ;

    this._anns.sort((e1, e2) =>
      e1[1] < e2[1] ? 1
        : e1[1] > e2[1] ? -1
          : e2[0] - e1[0]
    );
    this._wg
      .removeAll()
      .add($("table")
        .adds(this._anns.map(ann =>
          this._del.isNothing()
            ? $("tr")
              .adds(addAnn(ann))
            : $("tr")
              .add(Ui.link(() => {
                this._del.fromJust()(ann[0]);
              }).add(Ui.img("delete")))
              .adds(addAnn(ann)))))
    ;
  }
}
