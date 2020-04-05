// Copyright 10-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Main Fleas page.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Maybe from "../../dmjs/Maybe.js";
import {Menu} from "../../dmjs/Menu.js";
import Path from "../../data/Path.js"; //eslint-disable-line
import {VMenu} from "../../dmjs/VMenu.js";
import Fmodel from "../../data/flea/Fmodel.js";
import HidingMenu from "../../pgs/main/HidingMenu.js"; //eslint-disable-line
import {_} from "../../I18n.js";
import Overview from "./Overview.js";
import MainFtests from "./ftests/MainFtests.js";
import MainCharts from "./charts/MainCharts.js";

const $ = e => Ui.$(e);

/**
    Main Fleas page.
**/
export default class MainFleas {
  /**
      @private
      @param {!Client} client
      @param {!HidingMenu} upMenu
      @param {!Path} path
      @param {string} page
      @param {!Array<!Fmodel>} models
      @param {!Fmodel} model
      @param {!Domo} wgChild
  **/
  constructor (client, upMenu, path, page, models, model, wgChild) {
    this._client = client;
    this._upMenu = upMenu;
    this._path = path;
    this._page = page;
    this._models = models;
    this._model = model;
    this._wgChild = wgChild;

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    let mSel = this._models.find(e => e.id === this._model.id);
    if (mSel === undefined) mSel = this._models[0];
    const ops = this._models.map(e => VMenu.option(
      e.id,
      e.id,
      () => { this.onModelSelection(e.id) }
    ));
    const vmenu = new VMenu(ops, mSel.id);

    function tg (t) { return t + "&" + mSel.id }
    const lopts = [
      this._upMenu.button,
      Menu.separator2(),
      Menu.tlink(tg("Overview"), _("Overview"), "Fleas"),
      Menu.separator2(),
      Menu.tlink(tg("Ranking"), _("Ranking"), "Fleas"),
      Menu.separator(),
      Menu.tlink(tg("Bests"), _("Bests"), "Fleas"),
      Menu.separator(),
      Menu.tlink(tg("Pool"), _("Pool"), "Fleas"),
      Menu.separator(),
      Menu.tlink(tg("Charts"), _("Charts"), "Fleas"),
      Menu.separator2(),
      Menu.tlink(tg("Tests"), _("Tests"), "Fleas")
    ];

    const ropts = [];
    const menu = new Menu(lopts, ropts, tg(this._page));

    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style("width:5px;vertical-align:top").add(vmenu.wg))
          .add($("td").style("width:100%;vertical-align:top;")
            .add(menu.wg)
            .add(this._wgChild))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {string} id
      @return {void}
  **/
  onModelSelection (id) {
    location.assign(this._path.change(id).location);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Path} path
      @param {!HidingMenu} menu
      @return {!Promise<!Maybe<!MainFleas>>}
  **/
  static async mk (client, path, menu) {
    const rp = await client.send({
      "module": "Fleas",
      "source": "MainFleas",
      "rq": "models"
    });
    const models = rp["models"].map(e => Fmodel.fromJs(e));

    const page0 = path.nextPage("Overview");
    if (page0.isNothing()) return Maybe.nothing;
    const page = page0.fromJust();
    const newPath0 = path.add(page);

    const page00 = newPath0.nextPage(models[0].id);
    if (page00.isNothing()) return Maybe.nothing;
    const modelId = page00.fromJust();
    let model = models.find(e => e.id === modelId);
    if (model === undefined) model = models[0];
    const newPath = newPath0.add(model.id);

    switch (page) {
    case "Overview": {
      const pg = new Overview(model);
      return Maybe.just(new MainFleas(
        client, menu, newPath, page, models, model, pg.wg)
      );
    }
    case "Charts": {
      const pg = await MainCharts.mk(client, model, Maybe.nothing, false);
      if (pg.isNothing()) return Maybe.nothing;
      return Maybe.just(new MainFleas(
        client, menu, newPath, page, models, model, pg.fromJust().wg)
      );
    }
    case "Tests": {
      const pg = new MainFtests(client, model);
      return Maybe.just(new MainFleas(
        client, menu, newPath, page, models, model, pg.wg)
      );
    }
    }
    return Maybe.nothing;
  }

}


