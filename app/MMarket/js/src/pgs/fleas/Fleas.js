// Copyright 21-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Main Fleas page.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import DateDm from "../../dmjs/DateDm.js";
import B64 from "../../dmjs/B64.js";
import {Menu} from "../../dmjs/Menu.js";
import Dmenu from "../../wgs/Dmenu.js"; //eslint-disable-line
import {Table, Col} from "../../wgs/Table.js";
import {VMenu} from "../../dmjs/VMenu.js";
import Cts from "../../data/Cts.js";
import Fmodel from "../../data/flea/Fmodel.js";
import Eflea from "../../data/flea/Eflea.js";
import EfleaDate from "../../data/flea/EfleaDate.js";
import Frank from "../../data/flea/Frank.js";
import {_} from "../../I18n.js";
import Overview from "./overview/Overview.js";
import Ftests from "./ftests/Ftests.js";
import Ranking from "./ranking/Ranking.js";
import Charts from "./charts/Charts.js";

const $ = e => Ui.$(e);

function mkHeaderBase () {
  return [
    new Col(_("Nº"), Col.COUNTER, 0, false, false),
    new Col(_("Id"), Col.STRING, -1, true, false),
    new Col(_("Assets"), Col.NUMBER, 2, false, false),
    new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
    new Col(
      _("Pf. Var"), Col.NUMBER, 4,
      false, false
    ),
    new Col(
      _("Eval."), Col.NUMBER, 2,
      false, false
    ),
    new Col(
      _("Buys"), Col.NUMBER, 0,
      false, false
    ),
    new Col(
      _("Sells"), Col.NUMBER, 0,
      false, false
    )
  ];
}


/**
    Main Fleas page.
**/
export default class Fleas {
  /**
      @private
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @param {!Array<!Fmodel>} models
  **/
  constructor (wg, dmenu, lcPath, models) {
    this._wg = wg;
    this._dmenu = dmenu;
    this._models = models;

    if (lcPath.length === 0) lcPath.push(models[0].id);
    this._mSel = models.find(e => e.id === lcPath[0]);
    if (this._mSel === undefined) {
      this._mSel = models[0];
      lcPath.push(this._mSel.id);
    }

    if (lcPath.length === 1) lcPath.push("overview");
    this._lcPath = lcPath;

    switch (lcPath[1]) {
    case "overview":
    case "ranking":
    case "bests":
    case "pool":
    case "charts":
    case "tests":
      this._target = lcPath[1];
      break;
    default:
      this._target = "overview";
    }

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const tg = this._target;
    const wg = $("div");
    const dmenu = this._dmenu;
    const mSel = this._mSel;

    const ops = this._models.map((e, ix) => VMenu.option(
      e.id,
      e.id,
      () => { this.onModelSelection(ix) }
    ));
    ops.unshift(VMenu.separator());
    ops.unshift(VMenu.title(_("Models")));
    const vmenu = new VMenu(ops, mSel.id);

    const link = "fleas&" + this._lcPath[0];
    const lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("overview", _("Overview"), link),
      Menu.separator2(),
      Menu.tlink("ranking", _("Ranking"), link),
      Menu.separator(),
      Menu.tlink("bests", _("Bests"), link),
      Menu.separator(),
      Menu.tlink("pool", _("Pool"), link),
      Menu.separator(),
      Menu.tlink("charts", _("Charts"), link),
      Menu.separator2(),
      Menu.tlink("tests", _("Tests"), link)
    ];

    const ropts = [];
    dmenu.downMenu = new Menu(lopts, ropts, tg);

    switch (tg) {
    case "overview":
      new Overview(wg, mSel); // eslint-disable-line
      break;
    case "ranking":
      this.ranking(wg);
      break;
    case "bests":
      this.bests(wg);
      break;
    case "pool":
      this.pool(wg);
      break;
    case "charts":
      this.charts(wg);
      break;
    case "tests":
      new Ftests(wg, mSel); // eslint-disable-line
      break;
    default:
      new Overview(wg, mSel); // eslint-disable-line
      break;
    }

    this._wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("width:5px;vertical-align:top")
            .add(vmenu.wg))
          .add($("td").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
          .add($("td")
            .style("width:100%;vertical-align:top;")
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {number} id
      @return {void}
  **/
  onModelSelection (id) {
    this._mSel = this._models[id];
    this._lcPath[0] = this._mSel.id;
    this.view();
  }

  /**
    @private
    @param {!Domo} wg
  **/
  async ranking (wg) {
    wg
      .removeAll()
      .add($("div")
        .add(Ui.img("wait.gif")));

    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "fleas",
      "rq": "ranking",
      "modelId": this._mSel.id
    });
    const /**!Array<!Frank>**/ ranking =
      rp["ranking"].map(e => Frank.fromJs(e));
    const /**!Array<string>**/ parNames = rp["parNames"];
    const /**!Array<number>**/ parDecs = rp["parDecs"];

    const cols = mkHeaderBase();
    cols.splice(1, 0, new Col(
      "", Col.ICON, -1, true, false
    ));

    for (let i = 0; i < parNames.length; ++i) {
      cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
    }

    const self = this;
    //eslint-disable-next-line
    new Ranking(wg, cols, ranking, (e, i) =>
      (i === 2) ? self.chart(false, e) : self.chart(true, e)
    );
  }

  /**
    @private
    @param {!Domo} wg
  **/
  async bests (wg) {
    wg
      .removeAll()
      .add($("div")
        .add(Ui.img("wait.gif")));

    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "fleas",
      "rq": "bests",
      "modelId": this._mSel.id
    });
    const /**!Array<!EfleaDate>**/ bests =
      rp["bests"].map(e => EfleaDate.fromJs(e));
    const /**!Array<string>**/ parNames = rp["parNames"];
    const /**!Array<number>**/ parDecs = rp["parDecs"];

    const cols = mkHeaderBase();
    cols.splice(1, 0, new Col(
      _("Date"), Col.STRING, -1, false, false
    ));

    for (let i = 0; i < parNames.length; ++i) {
      cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
    }

    const table = bests.map(ed => {
      const dt = DateDm.fromStr(ed.date).toString();
      const e = ed.eflea;
      const r = [
        e, 0, dt, e.flea.name, e.assets, e.profitsAvg, e.profitsVa,
        e.ev * 1000, e.buys, e.sells
      ];
      e.flea.params.forEach(g => { r.push(g) });
      return r;
    });
    const self = this;
    //eslint-disable-next-line
    new Table(wg, cols, table, 2, (e, i) => self.chart(false, e));
  }

  /**
    @private
    @param {!Domo} wg
  **/
  async pool (wg) {
    wg
      .removeAll()
      .add($("div")
        .add(Ui.img("wait.gif")));

    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "fleas",
      "rq": "pool",
      "modelId": this._mSel.id
    });
    const /**!Array<!Eflea>**/ pool = rp["pool"].map(e => Eflea.fromJs(e));
    const /**!Array<string>**/ parNames = rp["parNames"];
    const /**!Array<number>**/ parDecs = rp["parDecs"];

    const cols = mkHeaderBase();

    for (let i = 0; i < parNames.length; ++i) {
      cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
    }

    const table = pool.map(e => {
      const r = [
        e, 0, e.flea.name, e.assets, e.profitsAvg, e.profitsVa,
        e.ev * 1000, e.buys, e.sells
      ];
      e.flea.params.forEach(g => { r.push(g) });
      return r;
    });
    const self = this;
    //eslint-disable-next-line
    new Table(wg, cols, table, 6, (e, i) => self.chart(false, e));
  }

  /**
    @private
    @param {!Domo} wg
  **/
  async charts (wg) {
    const modelId = this._lcPath[0];
    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "fleas",
      "rq": "bests",
      "modelId": modelId
    });
    const /**!Array<!EfleaDate>**/ bests =
      rp["bests"].map(e => EfleaDate.fromJs(e));
    const /**!Array<string>**/ parNames = rp["parNames"];
    const /**!Array<number>**/ parDecs = rp["parDecs"];

    let /** boolean */ isAssets = true;
    let eflea = bests[0].eflea;

    const args = this._lcPath.splice(2);
    if (args.length === 2) {
      isAssets = /**@type {boolean}*/(JSON.parse(args[0]));
      /** @suppress {checkTypes} */
      eflea = Eflea.fromJs(JSON.parse(B64.decode(args[1])));
    }

    //eslint-disable-next-line
    new Charts(wg, modelId, isAssets, parNames, parDecs, eflea);
  }

  /**
      @private
      @param {boolean} isSummary If is assets summary
      @param {!Eflea} eflea
      @return string
  **/
  chart (isSummary, eflea) {
    return `?fleas&${this._lcPath[0]}&charts` +
           `&${isSummary}&${B64.encode(JSON.stringify(eflea.toJs()))}`
    ;
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @return {!Promise<!Fleas>}
  **/
  static async mk (wg, dmenu, lcPath) {
    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "fleas",
      "rq": "models"
    });
    const models = rp["models"].map(e => Fmodel.fromJs(e));

    return new Fleas(wg, dmenu, lcPath, models);
  }

}


