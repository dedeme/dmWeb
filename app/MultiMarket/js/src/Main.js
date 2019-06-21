// Copyright 23-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import {I18n, _} from "./I18n.js";
import Bye from "./core/Bye.js";
import SysMain from "./sys/SysMain.js";
import FleasMain from "./fleas/FleasMain.js";

const app = "MultiMarket";
const version = "201903";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

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

    // VIEW --------
    // TTTTTTTTTTTTT

    this._credits = $("a")
      .att("href", "doc/about.html")
      .att("target", "blank");


    this._view = $("div");
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
  get wg () {
    return $("div")
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

  /** @return {!Promise} */
  async bye () {
    const rq = {
      "module": "sys",
      "source": "SysMain",
      "rq": "go",
      "option": SysMain.homePageId
    };
    await Main.client.send(rq);
    const rq2 = {
      "module": "logout"
    };
    await Main.client.send(rq2);

    new Bye(this).show();
  }

  /** @return {!Promise} */
  async update () {
    const self = this;

    async function go () {
      const rq = {
        "module": ".",
      };

      /** @type {!Object<string, string>} */
      const rp = await Main.client.rq(rq);

      if (rp["lang"] === "en") {
        I18n.en();
      } else {
        I18n.es();
      }

      self._credits.html("<small>" + _("Help & Credits") + "</small>");

      const url = Ui.url();
      const /** string */ module = url["0"] || "sys";
      if (module === "sys") {
        new SysMain(self).show();
      } else if (module === "market") {
        //      new MarketMain(self).run();
      } else if (module === "daily") {
        //      new MarketMain(self).run();
      } else if (module === "fleas") {
        new FleasMain(self).show();
      } else {
        alert("Module '" + module + "' is unknown");
        location.assign(Main.urlBase);
      }
    }

    const url = Ui.url();
    const /** string */ module = url["0"] || "sys";

    if (module === "sys") {
      const /** boolean */ ok = await Main.client.connect();
      if (ok) {
        go();
      } else {
        new Auth(this).show();
      }
    } else {
      go();
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

}
