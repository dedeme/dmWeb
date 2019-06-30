// Copyright 18-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Client from "../dmjs/Client.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";  //eslint-disable-line
import Menu from "../wgs/Menu.js";
import {_} from "../I18n.js";

import Downloader from "./wgs/Downloader.js";
import Annotations from "./Annotations.js";
import Companies from "./Companies.js";
import Balance from "./Balance.js";
import Trading from "./Trading.js";
import Profits from "./Profits.js";

const $ = Ui.$;

/** Acc Main page. */
export default class AccMain {

  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;

    // MODEL -------
    // TTTTTTTTTTTTT

    /** @type {string} */
    this._lang = main.lang;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._menu = new Menu(false);

    this._view = $("div");

  }

  /** @return {!Main} */
  get main () {
    return this._main;
  }

  get lang () {
    return this._lang;
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }


  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  mkMenu () {
    const menu = this._menu;
    menu.reset();

    menu.addRight(Menu.mkOption(
      AccMain.managementPageId, _("Management"),
      () => location.assign(Main.urlBase)
    ));

    menu.addLeft(Menu.mkLink(
      AccMain.annotationsPageId, _("Annotations"), "acc"));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkLink(AccMain.companiesPageId, _("Companies"), "acc"));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkLink(AccMain.balancePageId, _("Balance"), "acc"));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkLink(AccMain.tradingPageId, _("Trading"), "acc"));
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkLink(AccMain.profitsPageId, _("Profits"), "acc"));
    menu.addLeft(Menu.separator2());
    menu.addLeft(new Downloader(this).wg);

  }

  /** @return {void} */
  show () {
    this.mkMenu();
    this.main.view.removeAll()
      .add(this._menu.wg)
      .add(this.view)
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  update () {
    const /** string */ page = Ui.url()["1"] || AccMain.profitsPageId;

    if (page === AccMain.annotationsPageId) {
      new Annotations(this).show();
    } else if (page === AccMain.companiesPageId) {
      new Companies(this).show();
    } else if (page === AccMain.balancePageId) {
      new Balance(this).show();
    } else if (page === AccMain.tradingPageId) {
      new Trading(this).show();
    } else if (page === AccMain.profitsPageId) {
      new Profits(this).show();
    } else {
      alert("Option '" + page + "' is unknown");
      new Annotations(this).show();
    }

    this._menu.setSelected(page);
  }

  // STATIC --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {string} */
  static get annotationsPageId () {
    return "_annotations_";
  }

  /** @return {string} */
  static get managementPageId () {
    return "_management_";
  }

  /** @return {string} */
  static get companiesPageId () {
    return "_companies_";
  }

  /** @return {string} */
  static get balancePageId () {
    return "_balance_";
  }

  /** @return {string} */
  static get tradingPageId () {
    return "_trading_";
  }

  /** @return {string} */
  static get profitsPageId () {
    return "_profits_";
  }

}

