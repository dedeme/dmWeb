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

import Home from "./Home.js";
import Annotations from "./Annotations.js";


/** Main page. */
export default class Main {

  constructor () {
    const self = this;
    /**
     * @private
     * @type {!Dom}
     */
    this._dom = new Dom(this);

    /**
     * @private
     * @type {!Client}
     */
    this._client = new Client(false, Cts.APP, () => {
      new Expired(self).show();
    });

    /**
     * @private
     * @type {string}
     */
    this._user = "";

    /**
     * @private
     * @type {string}
     */
    this._year = "";

    /**
     * @private
     * @type {boolean}
     */
    this._closed = false;
  }

  /** @return {!Dom} Container for DOM objects. */
  get dom () {
    return this._dom;
  }

  /** @return {!Client} Application Client. */
  get client () {
    return this._client;
  }

  /** @return {string} Current user. */
  get user () {
    return this._user;
  }

  /** @return {string} Current year. */
  get year () {
    return this._year;
  }

  /** @return {boolean} False if this.year is the last year. */
  get closed () {
    return this._closed;
  }

  /** @param {string=} page */
  async run (page) {
    const self = this;

    const data = {
      "page": "main",
      "rq": "init"
    };
    const rp = await self._client.send(data);
    const conf = rp["conf"];

    if (conf["lang"] === "es") I18n.es();
    else I18n.en();

    this._user = conf["user"];
    this._year = conf["year"];
    this._closed = rp["closed"];

    if (page === undefined) {
      new Home(self).show();
    } else {
      this.go(page);
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
   */
  go (page) {
    switch (page) {
    case Cts.HOME_PAGE_ID:
      new Home(this).show();
      break;
    case Cts.ANNOTATIONS_PAGE_ID:
      new Annotations(this).show();
      break;
    case Cts.BACKUPS_PAGE_ID: {
      new Backups(this).show();
      break;
    }
    case Cts.SETTINGS_PAGE_ID:
      new Settings(this).show();
      break;
    default:
      throw("Page '" + page + "' is unknown");
    }
  }
}
