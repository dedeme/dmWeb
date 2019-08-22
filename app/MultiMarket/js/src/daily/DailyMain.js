// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Store from "../dmjs/Store.js";
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import Menu from "../wgs/Menu.js";
import {_} from "../I18n.js";

import Data from "./data/Data.js";
import Co from "./data/Co.js";
import Summary from "./Summary.js";
import Cos from "./Cos.js";
const selStore = "${app}__sel";

const $ = e => Ui.$(e);

/** DailyMain page. */
export default class DailyMain {

  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;
    this._lang = main.lang;

    // MODEL -------
    // TTTTTTTTTTTTT

    /** @type {Data} */
    this._data = null;

    /** @type {!Array<string>} */
    this._sel = [];

    this._page = null;

    this._interval = null;

    this._lastProfits = 0;

    this._lastTick = 16;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._menu = new Menu(false);

    this._body = $("div");

    this._dataSpan = $("span").html("");
  }

  /** @return {string} */
  get lang () {
    return this._lang;
  }

  /** @return {!Domo} */
  get body () {
    return this._body;
  }

  // MODEL ---------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

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

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  show () {
    this._menu.addLeft(Menu.mkLink(
      DailyMain.summaryPageId, _("Summary"), "daily"));
    this._menu.addLeft(Menu.separator());
    this._menu.addLeft(Menu.mkLink(
      DailyMain.portfolioPageId, _("Portfolio"), "daily"));
    this._menu.addLeft(Menu.separator());
    this._menu.addLeft(Menu.mkLink(
      DailyMain.allCosPageId, _("All CO's"), "daily"));
    this._menu.addLeft(Menu.separator());
    this._menu.addLeft(Menu.mkLink(
      DailyMain.selectionPageId, _("Selection"), "daily"));

    this._menu.addRight(Menu.mkOption(
      DailyMain.managementPageId, _("Management"),
      () => location.assign(Main.urlBase)
    ));
    this._menu.addRight(Menu.separator());
    this._menu.addRight(Menu.mkOption(
      DailyMain.updatePageId, _("Update"),
      () => this.updateCharts()
    ));
    this._menu.addRight(this._dataSpan);

    this._main.view.removeAll()
      .add(this._menu.wg)
      .add(this._body)
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  async update () {
    const self = this;

    const selVal = Store.take(selStore);
    this._sel = selVal === null
      ? []
      : /** @type {!Array<string>} */(JSON.parse(selVal))
    ;

    const rq = {
      "module": "daily",
      "rq": "idata"
    };
    const rp = await Main.client.rq(rq);
    this.data = new Data(rp);

    const /** string */ page = Ui.url()["1"] || DailyMain.summaryPageId;
    this._page = null;
    switch (page) {
    case DailyMain.summaryPageId:
      this._page = new Summary(this);
      this._page.show();
      break;
    case DailyMain.portfolioPageId:
      this._page = new Cos(self, 1);
      this._page.show();
      break;
    case DailyMain.allCosPageId:
      this._page = new Cos(self, 0);
      this._page.show();
      break;
    case DailyMain.selectionPageId:
      this._page = new Cos(self, 2);
      this._page.show();
      break;
    default:
      throw("Source '" + page + "' is unknown");
    }

    this.menuUpdate();
    this._menu.setSelected(page);

    this.timerUpdate();
  }

  /** @private */
  menuUpdate () {
    const lang = this._lang;
    const data = this._data;
    const mcos = data.cos;
    const cos = [...mcos.keys()].map(k => mcos.get(k));
    const [risk, profits] = Co.allRiskProfits(cos);
    const server = data.server;
    let state = data.state;

    if (state !== "Active" || this._lastProfits !== profits) {
      this._lastTick = 16;
    } else {
      this._lastTick -= 2;
      if (this._lastTick < 0) {
        this._lastTick = 0;
      }
    }
    this._lastProfits = profits;
    const ticks = "                · · · · · · · · "
      .substring(this._lastTick, this._lastTick + 16);

    function formatN (n) {
      if (lang === "es") {
        return new Dec(n, 2).toIso();
      }
      return new Dec(n, 2).toEn();
    }
    state = state === "Active" ? ticks
      : state === "Activating" ? _("Activating")
        : state === "Deactivating" ? _("Deactivating")
          : state === "Sleeping (1)" ? _("Sleeping (1)")
            : state === "Historic" ? _("Historic")
              : _("Sleeping (2)")
    ;
    this._dataSpan.html(
      state +
      "| " + formatN(risk) + " · " +
      formatN(profits) + " · <font color='00aa41'>" +
      formatN(profits - risk) + "</font> | " +
      server + " | "
    );
  }

  /** @private */
  timerUpdate () {
    const self = this;

    if (self._interval !== null) {
      clearInterval(self._interval);
    }
    self._interval = setInterval(async () => {
      if (self._page !== null) {
        const rq = {
          "module": "daily",
          "rq": "idata"
        };
        const rp = await Main.client.rq(rq);
        self.data = new Data(rp);
        self._page.showData();
      }
      self.menuUpdate();
    }, 15000);
  }

  /** @private */
  async updateCharts () {
    this._main.view.removeAll()
      .add($("div").style("text-align:center")
        .add(Ui.img("wait2.gif").klass("frame")))
    ;

    const rq = {
      "module": "daily",
      "rq": "update"
    };
    await Main.client.rq(rq);

    this._main.view.removeAll()
      .add(this._menu.wg)
      .add(this._body)
    ;
    this.update();
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
      this._page.showData();
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
      this._page.showData();
    }
  }

  /** @return {string} Id of Summary page */
  static get summaryPageId () {
    return "_sumary_";
  }

  /** @return {string} Id of Portfolio page */
  static get portfolioPageId () {
    return "_portfolio_";
  }

  /** @return {string} Id of 'All CO's' page */
  static get allCosPageId () {
    return "_allCos_";
  }

  /** @return {string} Id of 'Selection' page */
  static get selectionPageId () {
    return "_selection_";
  }

  /** @return {string} */
  static get updatePageId () {
    return "_update_";
  }

  /** @return {string} */
  static get managementPageId () {
    return "_management_";
  }

}
