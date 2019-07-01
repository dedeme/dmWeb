// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Client from "../dmjs/Client.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";  //eslint-disable-line
import Menu from "../wgs/Menu.js";
import Home from "./Home.js";
import Nicks from "./Nicks.js";
import Servers from "./Servers.js";
import Annotations from "./Annotations.js";
import Schedule from "./Schedule.js";
import Settings from "./Settings.js";
import Backups from "./Backups.js";
import {_} from "../I18n.js";

const $ = Ui.$;

/** Sys Main page. */
export default class SysMain {

  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;

    // MODEL -------
    // TTTTTTTTTTTTT


    // VIEW --------
    // TTTTTTTTTTTTT

    this._menu = new Menu(false);

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

  /** @return {void} */
  show () {
    const main = this.main;
    const menu = this._menu;

    menu.addLeft(Menu.mkOption(
      SysMain.homePageId, _("Home"), () => this.go(SysMain.homePageId))
    );
    menu.addLeft(Menu.separator2());
    menu.addLeft(Menu.mkOption(
      SysMain.nicksPageId, _("Nicks"), () => this.go(SysMain.nicksPageId))
    );
    menu.addLeft(Menu.separator());
    menu.addLeft(Menu.mkOption(
      SysMain.serversPageId, _("Servers"), () => this.go(SysMain.serversPageId))
    );
    menu.addLeft(Menu.separator2());
    menu.addLeft(Menu.mkOption(
      SysMain.annotationsPageId, _("Annotations"),
      () => this.go(SysMain.annotationsPageId)
    ));

    menu.addRight(Menu.mkClose(main.bye.bind(main)));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkOption(
      SysMain.settingsPageId, _("Settings"),
      () => this.go(SysMain.settingsPageId)
    ));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkOption(
      SysMain.backupsPageId, _("Backups"),
      () => this.go(SysMain.backupsPageId)
    ));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkOption(
      SysMain.schedulePageId, _("Schedule"),
      () => this.go(SysMain.schedulePageId)
    ));

    main.view.removeAll()
      .add(menu.wg)
      .add(this.view)
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "sys",
      "source": "SysMain",
      "rq": "idata"
    };
    const /** !Object<string, string> */ rp = await Main.client.send(rq);

    const /** string */ page = rp["page"] || SysMain.homePageId;
    this._menu.setSelected(page);

    if (page === SysMain.homePageId) {
      new Home(this).show();
    } else if (page === SysMain.nicksPageId) {
      new Nicks(this).show();
    } else if (page === SysMain.serversPageId) {
      new Servers(this).show();
    } else if (page === SysMain.annotationsPageId) {
      new Annotations(this).show();
    } else if (page === SysMain.schedulePageId) {
      new Schedule(this).show();
    } else if (page === SysMain.settingsPageId) {
      new Settings(this).show();
    } else if (page === SysMain.backupsPageId) {
      new Backups(this).show();
    } else {
      throw new Error("Unknown page '" + page + "'");
    }
  }

  /**
   * @param {string} page Page to go
   * @return {!Promise}
   */
  async go (page) {
    const rq = {
      "module": "sys",
      "source": "SysMain",
      "rq": "go",
      "option": page
    };
    await Main.client.send(rq);
    this.update();
  }

  // STATIC --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @return {string}
   */
  static get homePageId () {
    return "home";
  }

  /** @return {string} */
  static get nicksPageId () {
    return "nicks";
  }

  /** @return {string} */
  static get serversPageId () {
    return "servers";
  }

  /** @return {string} */
  static get annotationsPageId () {
    return "annotations";
  }

  /** @return {string} */
  static get editPageId () {
    return "edit";
  }

  /** @return {string} Id of settings page */
  static get settingsPageId () {
    return "settings";
  }

  /** @return {string} Id of backups page */
  static get backupsPageId () {
    return "backups";
  }

  /** @return {string} Id of backups page */
  static get schedulePageId () {
    return "schedule";
  }

}

