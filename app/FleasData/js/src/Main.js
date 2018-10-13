// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import {I18n} from "./I18n.js";

import Data from "./Data.js";

const app = "FleasData";
const version = "201810";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const settingsPageId = "settings";
const dataPageId = "data";

const ibexGroupId = "ibex";
const selGroupId = "sel";
const allGroupId = "all";

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
    this._client = new Client(app, () => {
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
    this._families = null;
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
      throw new Error("Conf has not been initialized");
    }
    return this._conf;
  }

  /** @return {Array<string>} Families name */
  get families () {
    if (this._families === null) {
      throw new Error("Families has not been initialized");
    }
    return this._families;
  }

  async run () {
    const self = this;

    const data = {
      "page": "main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);

    this._conf = rp["conf"];
    this._families = rp["families"];

    if (this._conf["lang"] === "es") I18n.es();
    else I18n.en();

    const page = this._conf["page"];
    const family = this._conf["family"];
    if (page === dataPageId) {
      const allData = {};
      for (let i = 0; i < 10; ++i) {
        const data = {
          "page": "main",
          "rq": "getData",
          "family": family === "" ? this._families[0] : family,
          "part": String(i)
        };
        const rp = await self._client.send(data);
        const d = rp["data"];
        for (const k of Object.keys(d)) {
          allData[k] = d[k];
        }
      }
      new Data(self, family, allData).show();
    } else if (page === settingsPageId) {
      new Settings(self).show();
    } else {
      throw("Page '" + page + "' is unknown");
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
      "page": "main",
      "rq": "logout"
    };
    await this.client.send(rq);
    new Bye(this).show();
  }

  /**
   * @param {string} page Page to go
   * @param {string} family Fleas family
   * @return {Promise}
   */
  async go (page, family) {
    const rq = {
      "page": "main",
      "rq": "setMenu",
      "targetPage": page,
      "family": family
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
  static get dataPageId () {
    return dataPageId;
  }

  /** @return {string} */
  static get ibexGroupId () {
    return ibexGroupId;
  }

  /** @return {string} */
  static get selGroupId () {
    return selGroupId;
  }

  /** @return {string} */
  static get allGroupId () {
    return allGroupId;
  }

}
