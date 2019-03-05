// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import {I18n} from "./I18n.js";

import Bests from "./pages/Bests.js";
import Charts from "./pages/Charts.js";
import Results from "./pages/Results.js";

const app = "FleasData";
const version = "201902";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const settingsPageId = "settings";
const bestsPageId = "best";
const chartsPageId = "charts";

/** Main page. */
export default class Main {

  constructor () {
    const self = this;
    /**
     * @private
     * @type {!Dom}
     */
    this._dom = new Dom(self);

    /**
     * @private
     * @type {!Client}
     */
    this._client = new Client(true, app, () => {
      new Expired(self).show();
    });

    /**
     * @private
     * @type {Object<string, string>}
     */
    this._conf = null;

    /**
     * @private
     * @type {Array<string>}
     */
    this._fmodels = null;
  }

  /** @return {!Dom} Container for DOM objects. */
  get dom () {
    return this._dom;
  }

  /** @return {!Client} Application Client. */
  get client () {
    return this._client;
  }

  /** @return {!Object<string, string>} Configuration data. */
  get conf () {
    if (this._conf === null) {
      throw new Error("Model has not been initialized");
    }
    return this._conf;
  }

  /** @return {!Array<string>} Flea model names. */
  get fmodels () {
    if (this._fmodels === null) {
      throw new Error("'fmodels' has not been initialized");
    }
    return this._fmodels;
  }

  async run () {
    const self = this;

    const data = {
      "source": "main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);

    this._conf = rp["db"];
    this._fmodels = rp["fmodels"];
    this._fmodels.sort();

    if (this._conf["lang"] === "es") I18n.es();
    else I18n.en();

    const page = this._conf["menu"];

    switch (page) {
    case bestsPageId:
      new Bests(self).show();
      break;
    case chartsPageId:
      new Charts(self).show();
      break;
    case settingsPageId:
      new Settings(self).show();
      break;
    default:
      if (this._fmodels.indexOf(page) !== -1) {
        new Results(self, page).show();
      } else {
        new Settings(self).show();
      }
    }
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
      "source": "main",
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
      "source": "main",
      "rq": "setMenu",
      "option": page
    };
    await this.client.send(rq);
    this.run();
  }

  // _______
  // statics
  // TTTTTTT

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
    return settingsPageId;
  }

  /** @return {string} Id of update page */
  static get bestsPageId () {
    return bestsPageId;
  }

  /** @return {string} Id of create page */
  static get chartsPageId () {
    return chartsPageId;
  }

}
