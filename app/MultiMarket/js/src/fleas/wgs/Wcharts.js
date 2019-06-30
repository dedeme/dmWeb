// Copyright 28-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";
import It from "../../dmjs/It.js";
import Dec from "../../dmjs/Dec.js";
import ModalBox from "../../dmjs/ModalBox.js";
import {_} from "../../I18n.js";

import Main from "../../Main.js";
import Operations from "../../wgs/Operations.js";
import Chart from "./Chart.js";

const $ = Ui.$;

function mkHistoric (box, div, nick, hs) {
  const bt = $("button").html("Close");
  div.removeAll()
    .add($("div").style("text-align:center").html(nick))
    .add(new Operations(hs).wg())
    .add($("hr"))
    .add(bt);
  bt.on("click", () => {
    box.show(false);
  });
}

function mkGrBig (box, div, nick, qs) {
  const bt = $("button").html("Close");
  div.removeAll()
    .add($("div").style("text-align:center").html(nick))
    .add(Chart.mk(true, qs))
    .add($("hr"))
    .add(bt);
  bt.on("click", () => {
    box.show(false);
  });
}

function mkGrTd (box, div, nick, profits, qs, hs) {
  if (nick === "") {
    return $("td").add($("table").att("align", "center").add($("tr")
      .add($("td").html(_("Without data")))));
  }
  return $("td")
    .add($("table").klass("main")
      .add($("tr")
        .add($("td").style("text-align:left;width:40%")
          .add($("span").html("&nbsp;&nbsp;"))
          .add($("span").html(nick)))
        .add($("td"))
        .add($("td").style("text-align:right;width:40%")
          .add($("span").html(new Dec(profits * 100, 2).toEu() + "%"))
          .add($("span").html("&nbsp;&nbsp;"))
          .add(Ui.img(profits < 0 ? "loose" : "win")
            .style("vertical-align:top")
            .setStyle("cursor", "pointer")
            .on("click", () => {
              mkHistoric(box, div, nick, hs);
              box.show(true);
            }))
          .add($("span").html("&nbsp;&nbsp;"))))
      .add($("tr")
        .add($("td").att("colspan", 3)
          .add(Chart.mk(false, qs).setStyle("cursor", "pointer")
            .on("click", () => {
              mkGrBig(box, div, nick, qs);
              box.show(true);
            })))))
  ;
}

/** Companies charts. */
export default class Wcharts {

  /**
   * @param {number} type BESTS, CHAMPIONS
   * @param {string} model Model to calculate charts
   * @param {!Array<string>} nicks Nicks list
   * @param {number=} group If 'type' is CHAMPIONS is the parameter number.
   * @param {string=} flea If 'type' is CHAMPIONS is the flea name
   *                       (date-cycle-id)
   */
  constructor (type, model, nicks, group, flea) {
    this._type = type;
    this._model = model;
    this._nicks = nicks;
    /** @type {number} */
    this._group = group || 0;
    /** @type {string} */
    this._flea = flea || "";

    // VIEW --------
    // TTTTTTTTTTTTT

    this._wg = $("div");
    this.update();
  }

  /** @return {!Domo} */
  get wg () {
    return this._wg;
  }

  /** @private */
  update () {
    const type = this._type;
    const model = this._model;
    const group = this._group;
    const flea = this._flea;
    const nicks = this._nicks;
    nicks.sort();

    const tb = $("table").klass("frame").att("align", "center");
    const rs = $("div");
    const div = $("div");
    const box = new ModalBox(div);

    this._wg.removeAll().add(
      $("div").style("text-align:center")
        .add(rs)
        .add(tb)
        .add(box.wg)
        .add(Ui.upTop("up"))
    );

    let tds = [];
    let ttProfits = 0;
    let positives = 0;
    let negatives = 0;
    const ncols = 3;
    It.from(nicks).eachSync(
      async (nick) => {
        const rq = {
          "module": "fleas",
          "source": "Wcharts",
          "rq": type, // BESTS (0), CHAMPIONS (1)
          "model": model,
          "nick": nick,
          "group": group,
          "flea": flea
        };
        const rp = await Main.client.rq(rq);
        const data = rp["data"];
        let profits = 0;
        let qs = [];
        let hs = [];
        if (data === null) {
          nick = ""; // Force an empty chart
        } else {
          profits = data[1];
          qs = data[2];
          hs = data[3];
        }
        ttProfits += profits;
        if (profits > 0) {
          ++positives;
        } else {
          ++negatives;
        }
        tds.push(mkGrTd(box, div, nick, profits, qs, hs));
        if (tds.length === ncols) {
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tb.add($("tr").adds(tds));
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tds = [];
        }
      },
      () => {
        if (tds.length > 0) {
          It.range(ncols - tds.length).each(() => { tds.push($("td")) });
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
          tb.add($("tr").adds(tds));
          tb.add($("tr").add($("td").att("colspan", ncols).add($("hr"))));
        }
        rs.html(
          "<img src='img/win.png' style='vertical-align:top'> : " +
          positives + " | " +
          "<img src='img/loose.png' style='vertical-align:top'> : " +
          negatives + " | " +
          "% : " +
          new Dec(ttProfits * 100 / (positives + negatives), 2).toEu());
      }
    );
  }

  /** @return {number} */
  static get BESTS () {
    return 0;
  }

  /** @return {number} */
  static get CHAMPIONS () {
    return 1;
  }


}
