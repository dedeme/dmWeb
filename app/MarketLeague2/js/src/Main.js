// Copyright 18-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Store from "./dmjs/Store.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import {I18n, _} from "./I18n.js";
import Bye from "./core/Bye.js";

import Menu from "./wgs/Menu.js";
import {Data} from "./Data.js";
import Settings from "./Settings.js";
import WRanking from "./wgs/WRanking.js";


const app = "MarketLeague";
const version = "201907";
const langStore = `${app}__lang`;
const captchaAuthStore = `${app}__captcha`;
const captchaChpassStore = `${app}__captchaCh`;

const appStore = `${app}__data`;

const $ = Ui.$;

let client = null;

/** Main page. */
export default class Main {

  constructor () {
    // MODEL -------
    // TTTTTTTTTTTTT

    client = new Client(true, app, () => {
      new Expired(this).show();
    });

    this._lang = "es";

    /** @type {Data} */
    this._data = null;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._credits = $("a")
      .att("href", "doc/about.html")
      .att("target", "blank");

    this._menuDiv = $("div");

    this._view = $("div");

    /** @type {!Menu} */
    this._menu = new Menu(false);
  }

  /** @return {string} */
  get lang () {
    return this._lang;
  }

  /** @return {!Domo} */
  get view () {
    return this._view;
  }

  /** @return {!Data} */
  get data () {
    if (this._data === null) {
      throw Error("'data' is null");
    } else {
      return this._data;
    }
  }

  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
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

  /** @return {void} */
  show () {
    $("@body").removeAll().add(this.wg);
    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  menu () {
    const m = this._menu;
    m.reset();

    m.addLeft(Menu.mkLink(Main.dailyPageId, _("Daily")));
    m.addLeft(Menu.separator());
    m.addLeft(Menu.mkLink(Main.shortPageId, _("Short")));
    m.addLeft(Menu.separator());
    m.addLeft(Menu.mkLink(Main.mediumPageId, _("Medium")));
    m.addLeft(Menu.separator());
    m.addLeft(Menu.mkLink(Main.longPageId, _("Long")));

    m.addRight(Menu.mkClose(this.bye.bind(this)));
    m.addRight(Menu.separator());
    m.addRight(Menu.mkLink(Main.settingsPageId, _("Settings")));

    this._menuDiv.removeAll().add(m.wg);
  }

  bye () {
    if (!confirm(_("Application exit?"))) {
      return;
    }
    const rq = {
      "rq": "logout"
    };
    Main.client.send(rq);

    new Bye(this).show();
  }

  /** @return {!Promise} */
  async update () {
    const /** boolean */ ok = await Main.client.connect();
    if (ok) {
      const rq = {
        "rq": "getLang",
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

      // Store.del(appStore);
      const data = Store.take(appStore);
      /** type {Data} */
      let d;
      if (data === null) {
        const rq = {
          "rq": "init"
        };
        const rp = await Main.client.rq(rq);
        const data = rp["data"];
        Store.put(appStore, JSON.stringify(data));
        d = Data.fromJs(data);
      } else {
        const rq = {
          "rq": "update",
          "data": JSON.parse(data)
        };
        const rp = await Main.client.rq(rq);
        const error = rp["error"];
        if (error !== "") {
          if (error.startsWith("replace")) {
            const parts = error.split(";");
            data.replace(parts[1], parts[2]);
            Store.put(appStore, JSON.stringify(data));
          } else {
            alert(error);
          }
          location.assign("");
        }
        const data2 = rp["data"];
        Store.put(appStore, JSON.stringify(data2));
        d = Data.fromJs(data2);
      }

      const url = Ui.url();
      const /** string */ module = url["0"] || Main.dailyPageId;
      if (module === Main.dailyPageId) {
        new WRanking(d.previous.dailyG, d.current.dailyG).show(this._view);
      } else if (module === Main.shortPageId) {
        new WRanking(d.previous.shortG, d.current.shortG).show(this._view);
      } else if (module === Main.mediumPageId) {
        new WRanking(d.previous.mediumG, d.current.mediumG).show(this._view);
      } else if (module === Main.longPageId) {
        new WRanking(d.previous.longG, d.current.longG).show(this._view);
      } else if (module === Main.settingsPageId) {
        new Settings(this).show();
      } else {
        alert("Module '" + module + "' is unknown");
        location.assign(Main.urlBase);
      }

      this._menu.setSelected(module);
    } else {
      new Auth(this).show();
    }
  }

  // STATIC --------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {string } */
  static get urlBase () {
    let path = window.location.href;
    const ix = path.indexOf("?");
    path = ix === -1 ? path : path.substring(0, ix);
    return path;
  }

  /** @return {string} Application name */
  static get app () {
    return app;
  }

  /** @return {string} Application version */
  static get version () {
    return version;
  }

  /** @return {string} Key for language data store */
  static get langStore () {
    return langStore;
  }

  /** @return {string} Key for authentication captcha data store */
  static get captchaAuthStore () {
    return captchaAuthStore;
  }

  /** @return {string} Key for change pass captcha data store */
  static get captchaChpassStore () {
    return captchaChpassStore;
  }

  /** @return {!Client} */
  static get client () {
    if (client === null) {
      throw new Error("Client is not initialized");
    }
    return client;
  }

  // ---------------------------------------------

  /** @return {string} Application name */
  static get dailyPageId () {
    return "_daily_";
  }

  /** @return {string} Application name */
  static get shortPageId () {
    return "_short_";
  }

  /** @return {string} Application name */
  static get mediumPageId () {
    return "_medium_";
  }

  /** @return {string} Application name */
  static get longPageId () {
    return "_long_";
  }

  /** @return {string} Application name */
  static get settingsPageId () {
    return "_settings_";
  }

}
