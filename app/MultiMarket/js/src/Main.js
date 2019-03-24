// Copyright 23-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import SysMain from "./sys/SysMain.js";
import {I18n, _} from "./I18n.js";

import Bye from "./core/Bye.js";
import Backups from "./core/Backups.js";

const app = "MultiMarket";
const version = "201903";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const $ = Ui.$;

/** Main page. */
export default class Main {

  constructor () {
    /**
     * @private
     * @type {!Dom}
     */
    this._dom = new Dom(this);

    /**
     * @private
     * @type {!Client}
     */
    this._client = new Client(true, app, () => {
      new Expired(this).show();
    });

    /**
     * @private
     * @type {Object<string, string>}
     */
    this._model = null;
  }

  /** @return {!Dom} Main container */
  get dom () {
    return this._dom;
  }

  /** @return {!Client} Application Client. */
  get client () {
    return this._client;
  }

  /** @return {!Object<string, string>} Configuration data. */
  get model () {
    if (this._model === null) {
      throw new Error("Model has not been initialized");
    }
    return this._model;
  }

  async run () {
    const rq = {
      "module": ".",
    };

    const rp = await this.client.sendAsync(rq);

    if (rp["lang"] === "en") {
      I18n.en();
    } else {
      I18n.es();
    }

    const url = Ui.url();
    const page = url["0"];
    if (page === undefined) {
      new SysMain(this).run();
    } else {
      let path = window.location.href;
      const ix = path.indexOf("?");
      path = ix === -1 ? path : path.substring(0, ix);
      location.assign(path);
    }
    /*
    const data = {
      "source": "main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);

    this._model = rp["db"];

    if (this._model["lang"] === "es") I18n.es();
    else I18n.en();

    const page = this._model["menu"];
    switch (page) {
    case backupsPageId: {
      new Backups(self).show();
      break;
    }
    case settingsPageId:
      new Settings(self).show();
      break;
    default:
      throw("Page '" + page + "' is unknown");
    }
    */
  }

  async start () {
    const ok = await this._client.connect();
    this._client.setPageId();

    if (ok) {
      this.run();
    } else {
      new Auth(this).show();
    }
  }

  // __________
  // Call backs
  // TTTTTTTTTT

  /** @return {Promise} */
  async bye () {
    const rq = {
      "module": "sys",
      "page": "Menu",
      "rq": "logout"
    };
    await this.client.send(rq);
    new Bye(this).show();
  }

  /**
   * @param {string} page Page to go
   * @return {Promise}
   */
  async go (page) {
    const rq = {
      "module": "sys",
      "page": "Menu",
      "rq": "go",
      "option": page
    };
    await this.client.send(rq);
    location.assign(Main.urlBase);
  }

  // _______
  // statics
  // TTTTTTT

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

  /** @return {string} Id of settings page */
  static get settingsPageId () {
    return "settings";
  }

  /** @return {string} Id of backups page */
  static get backupsPageId () {
    return "backups";
  }

}
