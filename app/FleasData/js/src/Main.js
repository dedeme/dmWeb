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
import Bests from "./Bests.js";

const app = "FleasData";
const version = "201810";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

const bestsPageId = "bests";
const settingsPageId = "settings";

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
      this._dom = new Dom(self);
      new Expired(self).show();
    });

    /**
     * @private
     * @type {Object<string, string>}
     */
    this._model = null;

    /**
     * @private
     * @type {Array<string>}
     */
    this._fgroups = null;

    /**
     * @private
     * @type {Object<string, string>}
     */
    this._constants = null;

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

  /** @return {!Array<string>} Sorted fleas groups. */
  get fgroups () {
    if (this._fgroups === null) {
      throw new Error("'fgroups' has not been initialized");
    }
    return this._fgroups;
  }

  /** @return {!Object<string, string>} Fleas constants data. */
  get constants () {
    if (this._constants === null) {
      throw new Error("'constants' has not been initialized");
    }
    return this._constants;
  }

  async run () {
    const self = this;

    const data = {
      "source": "main",
      "rq": "getDb"
    };
    const rp = await self._client.send(data);

    self._model = rp["db"];
    self._fgroups = rp["fgroups"];
    self._constants = rp["constants"];

    if (self._model["lang"] === "es") I18n.es();
    else I18n.en();

    self._dom = new Dom(self);

    const page = self._model["tmenu"];

    if (self.fgroups.indexOf(page) !== -1) {
      if (page === bestsPageId) {
        new Bests(self, self._model["fgroup"]).show();
      } else {
        new Data(self, page, self._model["lmenu"]).show();
      }
    } else if (page === settingsPageId) {
      new Settings(self).show();
    } else {
      new Settings(self).show();
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
   * @param {string} tmenu Top menu option.
   * @param {string} lmenu Left menu option.
   *    If tmenu is different to a fleas group, its value is "".
   * @param {string} fgroup Left menu oprion.
   *    If tmenu is different to 'bests', its value is "".
   * @return {Promise}
   */
  async go (tmenu, lmenu, fgroup) {
    const rq = {
      "source": "main",
      "rq": "setMenu",
      "tmenu": tmenu,
      "lmenu": lmenu,
      "fgroup": fgroup
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

  /** @return {string} Id of best page */
  static get bestsPageId () {
    return bestsPageId;
  }

  /** @return {string} Id of settings page */
  static get settingsPageId () {
    return settingsPageId;
  }

}
