// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js";
import Store from "./dmjs/Store.js";
import Dom from "./core/Dom.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import Settings from "./core/Settings.js";
import Bye from "./core/Bye.js";
import {I18n} from "./I18n.js";
import Data from "./data/Data.js";

import Summary from "./Summary.js";
import Cos from "./Cos.js";
import Log from "./Log.js";
const app = "DailyMarket";
const version = "201901";
const langStore = "${app}__lang";
const captchaAuthStore = "${app}__captcha";
const captchaChpassStore = "${app}__captchaCh";
const selStore = "${app}__sel";

const settingsPageId = "settings";
const summaryPageId = "summary";
const portfolioPageId = "portfolio";
const allCosPageId = "allcos";
const selectionPageId = "selection";
const logPageId = "log";

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

    /**
     * @private
     * @type {Data}
     */
    this._data = null;

    /**
     * @private
     */
    this._sel = [];

    /**
     * @private
     */
    this._page = null;

    /** @private */
    this._interval = null;
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

    const selVal = Store.take(selStore);
    this._sel = selVal === null ? [] : JSON.parse(selVal);

    let rq = {
      "source": "reader",
      "rq": "read"
    };
    let rp = await this.client.sendAsync(rq);
    this.data = new Data(rp);

    rq = {
      "source": "main",
      "rq": "getDb"
    };
    rp = await self._client.sendAsync(rq);

    this._model = rp["db"];

    if (this._model["lang"] === "es") I18n.es();
    else I18n.en();

    const page = this._model["menu"];
    this._page = null;
    switch (page) {
    case summaryPageId:
      this._page = new Summary(self);
      this._page.show();
      break;
    case portfolioPageId:
      this._page = new Cos(self, 1);
      this._page.show();
      break;
    case allCosPageId:
      this._page = new Cos(self, 0);
      this._page.show();
      break;
    case selectionPageId:
      this._page = new Cos(self, 2);
      this._page.show();
      break;
    case logPageId:
      new Log(self).show();
      break;
    case settingsPageId:
      new Settings(self).show();
      break;
    default:
      throw("Source '" + page + "' is unknown");
    }
    this.update();
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
    await this.client.sendAsync(rq);
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
    await this.client.sendAsync(rq);
    this.run();
  }

  update () {
    if (this._interval !== null) {
      clearInterval(this._interval);
    }
    this._interval = setInterval(async () => {
      if (this._page !== null) {
        const rq = {
          "source": "reader",
          "rq": "read"
        };
        const rp = await this.client.sendAsync(rq);
        this.data = new Data(rp);
        this._page.showData();
      }
      this._dom.update();
    }, 15000);
  }

  /**
   * @param {string} nick
   * @return {boolean}
   */
  isSel (nick) {
    return this._sel.findIndex(e => e === nick) !== -1;
  }

  /**
   * @param {string} nick
   * @return {void}
   */
  addSel (nick) {
    if (!this.isSel(nick)) {
      this._sel.push(nick);
      Store.put(selStore, JSON.stringify(this._sel));
      this.run();
    }
  }

  /**
   * @param {string} nick
   * @return {void}
   */
  removeSel (nick) {
    const ix = this._sel.findIndex(e => e === nick);
    if (ix !== -1) {
      this._sel.splice(ix, 1);
      Store.put(selStore, JSON.stringify(this._sel));
      this.run();
    }
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

  /** @return {string} Id of Summary page */
  static get summaryPageId () {
    return summaryPageId;
  }

  /** @return {string} Id of Portfolio page */
  static get portfolioPageId () {
    return portfolioPageId;
  }

  /** @return {string} Id of 'All CO's' page */
  static get allCosPageId () {
    return allCosPageId;
  }

  /** @return {string} Id of 'Selection' page */
  static get selectionPageId () {
    return selectionPageId;
  }

  /** @return {string} Id of 'Log' page */
  static get logPageId () {
    return logPageId;
  }

  /**
   * Returns server data
   * @return {!Data}
   */
  get data () {
    if (this._data === null) {
      throw(new Error("'Main.data' is not intialized"));
    }
    return this._data;
  }

  /**
   * Sets server data
   * @param {!Data} d
   */
  set data (d) {
    this._data = d;
  }
}
