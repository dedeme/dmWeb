// Copyright 06-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import Backups from "./core/Backups.js";
import {I18n} from "./I18n.js";
import Cts from "./Cts.js";

import Update from "./Update.js";
import Create from "./Create.js";


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
    this._client = new Client(false, Cts.APP, () => {
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
      "rq": "init"
    };
    const rp = await self._client.send(data);

    self._model = rp["conf"];

    if (self._model["lang"] === "es") I18n.es();
    else I18n.en();

    const page = self._model["menu"];
    switch (page) {
    case Cts.UPDATE_PAGE_ID:
      new Update(self).show();
      break;
    case Cts.CREATE_PAGE_ID:
      new Create(self).show();
      break;
    case Cts.BACKUPS_PAGE_ID: {
      new Backups(self).show();
      break;
    }
    case Cts.SETTINGS_PAGE_ID:
      new Settings(self).show();
      break;
    default:
      throw("Page '" + page + "' is unknown");
    }
  }

  async start () {
    const ok = await this._client.connect();
    this._client.setPageId();
    if (ok) {
      this.run();
    } else {
      await this._client.authentication("", "", false); // Dummy values
      this.start();
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
}
