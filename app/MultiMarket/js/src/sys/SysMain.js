// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "../dmjs/Client.js";
import Dom from "../core/Dom.js";
import Main from "../Main.js";
import Home from "./Home.js";
import Settings from "./Settings.js";
import {_} from "../I18n.js";

/** Sys Main page. */
export default class SysMain {
  /**
   * @param {!Main} main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    const dom = main.dom;
    dom.resetMenu();
    dom.addLeftMenu(Dom.mkOption(
      SysMain.homePageId, _("Home"), () => this.go(SysMain.homePageId))
    );
    dom.addLeftMenu(Dom.separator());
    dom.addLeftMenu(Dom.mkOption(
      SysMain.settingsPageId, _("Settings"),
      () => this.go(SysMain.settingsPageId)
    ));
    dom.addLeftMenu(Dom.separator());
    dom.addLeftMenu(Dom.mkOption(
      SysMain.backupsPageId, _("Backups"),
      () => this.go(SysMain.backupsPageId)
    ));
  }

  /** @return {!Main} */
  get main () {
    return this._main;
  }

  /** @return {!Client} */
  get client () {
    return this._main.client;
  }

  /** @return {!Dom} */
  get dom () {
    return this._main.dom;
  }

  async run () {
    const rq = {
      "module": "sys",
      "page": "SysMain",
      "rq": "idata"
    };

    const rp = await this.client.sendAsync(rq);

    const page = rp["page"] || SysMain.homePageId;

    if (page === SysMain.homePageId) {
      new Home(this).show();
    } else if (page === SysMain.settingsPageId) {
      new Settings(this).show();
    } else {
      throw new Error("Unknown page '" + page + "'");
    }
  }

  /**
   * @param {string} page Page to go
   * @return {Promise}
   */
  async go (page) {
    const rq = {
      "module": "sys",
      "page": "SysMain",
      "rq": "go",
      "option": page
    };
    await this.client.send(rq);
    this.run();
  }

  /** @return {string} */
  static get homePageId () {
    return "home";
  }

  /** @return {string} Id of settings page */
  static get settingsPageId () {
    return "settings";
  }

  /** @return {string} Id of backups page */
  static get backupsPageId () {
    return "backups";
  }

}

