// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "../Main.js";
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import ModalBox from "../dmjs/ModalBox.js";
import Lmenu from "../widgets/Lmenu.js";
import Chart from "../widgets/Chart.js";
import Operations from "../widgets/Operations.js";

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
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    const body = $("div");
    const client = this._main.client;

    /**
     * @param {string} model
     * @return {Promise<?>}
     */
    async function selectModel (model) {
      const rq = {
        "source": "charts",
        "rq": "setModel",
        "model": model
      };
      await client.send(rq);

      const tb = $("table").klass("frame").att("align", "center");
      const rs = $("div");
      const div = $("div");
      const box = new ModalBox(div);

      body.removeAll().add(
        $("div").style("text-align:center")
          .add(rs)
          .add(tb)
          .add(box.wg)
          .add(Ui.upTop("up"))
      );

      const rq2 = {
        "source": "charts",
        "rq": "list",
        "fmodel": model
      };
      const rp = await client.send(rq2);
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
            "source": "charts",
            "rq": "data",
            "fmodel": model,
            "nick": nick
          };
          const rp = await client.send(rq);
          const data = rp["data"];
          const profits = data[1];
          const qs = data[2];
          const hs = data[3];
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

    const main = this._main;
    const fmodels = main.fmodels;

    const rq = {
      "source": "charts",
      "rq": "getModel"
    };
    const rp = await client.send(rq);
    let model = rp["model"];
    if (model === "") {
      model = fmodels[0];
    }

    main.dom.show(Main.chartsPageId, $("div")
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top;")
            .add(new Lmenu(fmodels, selectModel, model).wg()))
          .add($("td").style("vertical-align:top;")
            .add(body))))
      .add(Ui.upTop("up"))
    );

    selectModel(model);
  }
}

