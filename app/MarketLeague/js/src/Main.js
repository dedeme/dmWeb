// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Menu from "./dmjs/Menu.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import {I18n, _} from "./I18n.js";
import Bye from "./core/Bye.js";

import Settings from "./Settings.js";
import Backups from "./Backups.js";
import Leagues from "./Leagues.js";


const app = "MarketLeague";
const version = "201908";
const langStore = `${app}__lang`;
const captchaAuthStore = `${app}__captcha`;
const captchaChpassStore = `${app}__captchaCh`;

const $ = e => Ui.$(e);

let client = null;

/**
    Main page.
**/
export default class Main {

  constructor () {
    client = new Client(true, app, () => {
      new Expired(this).show();
    });
    this._lang = "es";
    this._credits = $("a")
      .att("href", "doc/about.html")
      .att("target", "blank");
    this._menuDiv = $("div");
    this._view = $("div");
    /**
        @type {!Menu}
    **/
    this._menu = new Menu(false);
  }

  /**
      @return {string}
  **/
  get lang () {
    return this._lang;
  }

  /**
      @return {!Domo}
  **/
  get view () {
    return this._view;
  }

  // VIEW ----------------------------------------------------------------------

  /**
      @private
  **/
  get wg () {
    return $("div")
      .add(this._menuDiv)
      .add(this._view)
      .add($("p").html("&nbsp;"))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").add(this._credits))
          .add($("td")
            .style("text-align: right;font-size: 10px;" +
              "color:#808080;font-size:x-small;")
            .html(`- © ºDeme. ${Main.app} (${Main.version}) -`))))
    ;
  }

  /**
      @return {void}
  **/
  show () {
    $("@body").removeAll().add(this.wg);
    this.update();
  }

  // CONTROL -------------------------------------------------------------------

  /**
      @private
  **/
  menu () {
    const m = this._menu;
    m.reset();

    m.addLeft(Menu.mkLink(Main.leaguesPageId, _("Leagues")));

    m.addRight(Menu.mkClose(this.bye.bind(this)));
    m.addRight(Menu.separator());
    m.addRight(Menu.mkLink(Main.settingsPageId, _("Settings")));
    m.addRight(Menu.separator());
    m.addRight(Menu.mkLink(Main.backupsPageId, _("Backups")));

    this._menuDiv.removeAll().add(m.wg);
  }

  bye () {
    if (!confirm(_("Application exit?"))) {
      return;
    }
    const rq = {
      "page": "logout"
    };
    Main.client.send(rq);

    new Bye(this).show();
  }

  /**
      @return {!Promise}
  **/
  async update () {
    const /** boolean */ ok = await Main.client.connect();
    if (ok) {
      const rq = {
        "page": "Main",
        "rq": "lang"
      };
      /** @type {!Object<string, string>} */
      const rp = await Main.client.rq(rq);
      this._lang = rp["lang"] || "es";

      if (this._lang === "en") {
        I18n.en();
      } else {
        I18n.es();
      }

      this.menu();
      this._credits.html("<small>" + _("Help & Credits") + "</small>");

      const url = Ui.url();
      const /** string */ module = url["0"] || Main.leaguesPageId;
      if (module === Main.settingsPageId) {
        new Settings(this).show();
      } else if (module === Main.backupsPageId) {
        new Backups(this).show();
      } else if (module === Main.leaguesPageId) {
        new Leagues(this).show();
      } else {
        alert("Module '" + module + "' is unknown");
        location.assign(Main.urlBase);
      }

      this._menu.setSelected(module);
    } else {
      new Auth(this).show();
    }
  }

  // STATIC --------------------------------------------------------------------

  /**
      @return {string }
  **/
  static get urlBase () {
    let path = window.location.href;
    const ix = path.indexOf("?");
    path = ix === -1 ? path : path.substring(0, ix);
    return path;
  }

  /**
      @return {string} Application name
  **/
  static get app () {
    return app;
  }

  /**
      @return {string} Application version
  **/
  static get version () {
    return version;
  }

  /**
      @return {string} Key for language data store
  **/
  static get langStore () {
    return langStore;
  }

  /**
      @return {string} Key for authentication captcha data store
  **/
  static get captchaAuthStore () {
    return captchaAuthStore;
  }

  /**
      @return {string} Key for change pass captcha data store
  **/
  static get captchaChpassStore () {
    return captchaChpassStore;
  }

  /**
      @return {!Client}
  **/
  static get client () {
    if (client === null) {
      throw new Error("Client is not initialized");
    }
    return client;
  }

  /**
      @return {string}
  **/
  static get settingsPageId () {
    return "_settings_";
  }

  /**
      @return {string}
  **/
  static get backupsPageId () {
    return "_backups_";
  }

  /**
      @return {string}
  **/
  static get leaguesPageId () {
    return "_leagues_";
  }

}
