// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import FleasMain from "./FleasMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import It from "../dmjs/It.js";
import ModalBox from "../dmjs/ModalBox.js";
import Lmenu from "./wgs/Lmenu.js";
import Chart from "./wgs/Chart.js";
import Operations from "../wgs/Operations.js";

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

/** Charts page. */
export default class Charts {

  /**
   * @param {!FleasMain} fleasMain Main
   * @param {!Array<string>} models Flea models
   */
  constructor (fleasMain, models) {
    this._fleasMain = fleasMain;
    this._models = models;

    // VIEW ------------------
    // TTTTTTTTTTTTTTTTTTTTTTT
    this._left = $("div");
    this._right = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {void}
   */
  show () {
    this._fleasMain.view.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top;")
            .add(this._left)) //new Lmenu(fmodels, selectModel, model).wg()))
          .add($("td").style("vertical-align:top;")
            .add(this._right))))
      .add(Ui.upTop("up"))
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {Promise}
   */
  async update () {
    const self = this;

    /**
     * @param {string} model
     * @return {Promise<?>}
     */
    async function selectModel (model) {
      const rq = {
        "module": "fleas",
        "source": "charts",
        "rq": "setModel",
        "model": model
      };
      await Main.client.rq(rq);

      const tb = $("table").klass("frame").att("align", "center");
      const rs = $("div");
      const div = $("div");
      const box = new ModalBox(div);

      self._right.removeAll().add(
        $("div").style("text-align:center")
          .add(rs)
          .add(tb)
          .add(box.wg)
          .add(Ui.upTop("up"))
      );

      const rq2 = {
        "module": "fleas",
        "source": "charts",
        "rq": "nicks",
        "model": model
      };
      const rp = await Main.client.rq(rq2);
      const list = rp["list"];
      list.sort();

      let tds = [];
      let ttProfits = 0;
      let positives = 0;
      let negatives = 0;
      const ncols = 3;
      It.from(list).eachSync(
        async (nick) => {
          const rq = {
            "module": "fleas",
            "source": "charts",
            "rq": "data",
            "model": model,
            "nick": nick
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

    const rq = {
      "module": "fleas",
      "source": "charts",
      "rq": "getModel"
    };
    const rp = await Main.client.rq(rq);
    let /** string */ model = rp["model"] || this._models[0];
    if (!this._models.includes(model)) {
      model = this._models[0];
    }

    this._left.removeAll().add(
      new Lmenu(this._models, selectModel.bind(this), model).wg
    );

    await selectModel(model);
  }
 }
