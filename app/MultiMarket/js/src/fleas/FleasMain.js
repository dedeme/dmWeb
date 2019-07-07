// Copyright 18-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Client from "../dmjs/Client.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";  //eslint-disable-line
import Menu from "../wgs/Menu.js";
import {_} from "../I18n.js";
import Bests from "./Bests.js";
import Champions from "./Champions.js";
import Charts from "./Charts.js";
import Model from "./Model.js";

const $ = Ui.$;

/** Fleas Main page. */
export default class FleasMain {

  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;

    // MODEL -------
    // TTTTTTTTTTTTT


    // VIEW --------
    // TTTTTTTTTTTTT

    this._menu = new Menu(true);

    this._view = $("div");

  }

  /** @return {!Main} */
  get main () {
    return this._main;
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }

  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @param {!Array<string>} models
   */
  mkMenu (models) {
    const menu = this._menu;
    menu.reset();

    menu.addRight(Menu.mkOption(
      FleasMain.managementPageId, _("Management"),
      () => location.assign(Main.urlBase)
    ));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkLink(FleasMain.chartsPageId, _("Charts"), "fleas"));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkLink(FleasMain.bestsPageId, _("Bests"), "fleas"));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkLink(
      FleasMain.championsPageId, _("Champions"), "fleas"
    ));

    const len = models.length;
    if (len === 0) {
      return;
    }
    const m = models[0];
    menu.addLeft(Menu.mkLink(m, m, "fleas"));
    if (len === 1) {
      return;
    }
    for (let i = 1; i < len; ++i) {
      const m = models[i];
      menu.addLeft(Menu.separator());
      menu.addLeft(Menu.mkLink(m, m, "fleas"));
    }
  }

  /** @return {void} */
  show () {
    this.main.view.removeAll()
      .add(this._menu.wg)
      .add(this.view)
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "fleas",
      "source": "FleasMain",
      "rq": "idata"
    };
    const /** !Object<string, ?> */ rp = await Main.client.rq(rq);

    const /** string */ page = Ui.url()["1"] || FleasMain.championsPageId;
    const /** !Object<string, !Array<?>> */ pnames = rp["pnames"];
    const /** !Array<string> */ models = Object.keys(pnames);
    models.sort();

    this.mkMenu(models);
    this._menu.setSelected(page);

    if (page === FleasMain.championsPageId) {
      new Champions(this, pnames).show();
    } else if (page === FleasMain.chartsPageId) {
      new Charts(this, models).show();
    } else if (page === FleasMain.bestsPageId) {
      new Bests(this, models).show();
    } else {
      new Model(this, page).show();
    }
  }

  // STATIC --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {string} */
  static get championsPageId () {
    return "_champions_";
  }

  /** @return {string} */
  static get bestsPageId () {
    return "_bests_";
  }

  /** @return {string} */
  static get chartsPageId () {
    return "_charts_";
  }

  /** @return {string} */
  static get managementPageId () {
    return "_management_";
  }

}
