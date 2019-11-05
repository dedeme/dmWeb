// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./Dom.js";
import Expired from "./pages/Expired.js";
import Auth from "./pages/Auth.js";
import Settings from "./pages/Settings.js";
import Bye from "./pages/Bye.js";
import Backups from "./pages/Backups.js";
import {I18n} from "./I18n.js";

import Update from "./Update.js";
import Create from "./Create.js";

const app = "Dummy2";
const version = "201909";
const langStore = `${app}__lang`;
const captchaAuthStore = `${app}__captcha`;
const captchaChpassStore = `${app}__captchaCh`;

const settingsPageId = "settings";
const backupsPageId = "backups";
const updatePageId = "update";
const createPageId = "create";

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
      "page": "Main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);

    this._model = rp["db"];

    if (this._model["lang"] === "es") I18n.es();
    else I18n.en();

    const page = this._model["menu"];
    switch (page) {
    case updatePageId:
      new Update(self).show();
      break;
    case createPageId:
      new Create(self).show();
      break;
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
  }

  async start () {
    const ok = await this._client.connect();
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
  bye () {
    const rq = {
      "page": "logout"
    };
    this.client.send(rq);
    new Bye(this).show();
  }

  /**
   * @param {string} page Page to go
   * @return {Promise}
   */
  async go (page) {
    const rq = {
      "page": "Main",
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

  /** @return {string} Id of update page */
  static get updatePageId () {
    return updatePageId;
  }

  /** @return {string} Id of create page */
  static get createPageId () {
    return createPageId;
  }

}
