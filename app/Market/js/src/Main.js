// Copyright 12-Dic-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import Backups from "./core/Backups.js";
import {I18n} from "./I18n.js";

import Balance from "./pages/Balance.js";
import Annotations from "./pages/Annotations.js";
import Trading from "./pages/Trading.js";
import Graphics from "./pages/Graphics.js";

const app = "Market";
const version = "201812";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const settingsPageId = "settings";
const backupsPageId = "backups";

const balancePageId = "balance";
const annotationsPageId = "annotations";
const tradingPageId = "trading";
const graphicsPageId = "graphics";

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
    this._model = null;
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
  get model () {
    if (this._model === null) {
      throw new Error("Model has not been initialized");
    }
    return this._model;
  }

  async run () {
    const self = this;

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
    case balancePageId:
      new Balance(self).show();
      break;
    case annotationsPageId:
      new Annotations(self).show();
      break;
    case tradingPageId:
      new Trading(self).show();
      break;
    case graphicsPageId:
      new Graphics(self).show();
      break;
    case backupsPageId: {
      new Backups(self).show();
      break;
    }
    case settingsPageId:
      new Settings(self).show();
      break;
    default:
      throw("Source '" + page + "' is unknown");
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

  /** @return {string} Id of backups page */
  static get backupsPageId () {
    return backupsPageId;
  }

  /** @return {string} Id of Balance page */
  static get balancePageId () {
    return balancePageId;
  }

  /** @return {string} Id of Annotations page */
  static get annotationsPageId () {
    return annotationsPageId;
  }

  /** @return {string} Id of Trading page */
  static get tradingPageId () {
    return tradingPageId;
  }

  /** @return {string} Id of Graphics page */
  static get graphicsPageId () {
    return graphicsPageId;
  }

}
