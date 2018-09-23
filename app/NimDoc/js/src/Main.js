// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Bye from "./core/Bye.js";
import {I18n} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

import {MenuPath, Model} from "./Model.js";
import Paths from "./Paths.js";
import Index from "./Index.js";
import Module from "./Module.js";
import Code from "./Code.js";

const app = "NimDoc";
const version = "201809";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";

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
    this._client = new Client(app, () => {
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
      "page": "main",
      "rq": "idata"
    };
    const rp = await self._client.send(data);
    const locationPath = rp["path"];
    const lang = rp["lang"];
    const showAll = rp["show"];
    const ps = rp["paths"].map(p =>
      new MenuPath(p[0], p[1], p[2], p[3])
    ).sort((a, b) => a.id.localeCompare(b.id));

    if (lang === "es") I18n.es();
    else I18n.en();

    self._model = new Model(locationPath, lang, showAll, ps);

    const url = Ui.url();
    const path = url["0"];
    if (!path) {
      location.assign("?" + locationPath);
    } else if (path === "@") {
      self._model.sel = "@";
      new Paths(self).show();
    } else if (path.indexOf("@") === -1) {
      self._model.sel = path;
      new Index(self).show();
    } else {
      const parts = path.split("@");
      self.model.sel = parts[0];
      self.model.module = parts[1];
      if (url["1"] === undefined) {
        new Module(self).show();
      } else {
        self.model.link = url["1"];
        new Code(self).show();
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
      "page": "main",
      "rq": "logout"
    };
    await this.client.send(rq);
    new Bye(this).show();
  }

  /**
   * @param {string} path Path to go
   * @return {Promise}
   */
  async go (path) {
    const rq = {
      "page": "main",
      "rq": "go",
      "path": path
    };
    await this.client.send(rq);
    location.assign("?" + path);
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
