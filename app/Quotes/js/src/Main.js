// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import Backups from "./core/Backups.js";
import {I18n} from "./I18n.js";

import Nicks from "./Nicks.js";
import Edit from "./Edit.js";
import Issues from "./Issues.js";
import Servers from "./Servers.js";

const app = "Quotes";
const version = "201809";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const settingsPageId = "settings";
const backupsPageId = "backups";
const nicksPageId = "nicks";
const editPageId = "edit";
const issuesPageId = "issues";
const serversPageId = "servers";

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

    function hub (page) {
      switch (page) {
      case nicksPageId:
        new Nicks(self).show();
        break;
      case editPageId:
        new Edit(self).show();
        break;
      case issuesPageId:
        new Issues(self).show();
        break;
      case serversPageId:
        new Servers(self).show();
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

    const data = {
      "page": "main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);
    self._model = rp["db"];

    if (self._model["lang"] === "es") I18n.es();
    else I18n.en();

    const page = self._model["menu"];
    hub(page);
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
   * @return {Promise}
   */
  async go (page) {
    const rq = {
      "page": "main",
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

  /** @return {string} Id of Nicks page */
  static get nicksPageId () {
    return nicksPageId;
  }

  /** @return {string} Id of Edit page */
  static get editPageId () {
    return editPageId;
  }

  /** @return {string} Id of Issues page */
  static get issuesPageId () {
    return issuesPageId;
  }

  /** @return {string} Id of Servers page */
  static get serversPageId () {
    return serversPageId;
  }

}