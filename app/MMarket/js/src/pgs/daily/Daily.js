// Copyright 07-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Daily charts, main page.
**/

import Domo from "../../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../../dmjs/Ui.js";
import Dec from "../../dmjs/Dec.js";
import Maybe from "../../dmjs/Maybe.js";
import Store from "../../dmjs/Store.js";
import Cts from "../../data/Cts.js";
import {Menu, MenuEntry} from "../../dmjs/Menu.js";
import Dmenu from "../../wgs/Dmenu.js"; //eslint-disable-line
import {DailyChart} from "../../data/DailyChart.js";
import {_} from "../../I18n.js";
import Cos from "./Cos.js";
import Summary from "./Summary.js";

const $ = e => Ui.$(e);
const actKey = "MMarket_activity";
const ticKey = "MMarket_tic";
const ticIdKey = "MMarket_ticId";

export default class Daily {
  /**
      @private
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @param {string} server
      @param {string} activity
      @param {!Array<!DailyChart>} chartsData
  **/
  constructor (wg, dmenu, lcPath, server, activity, chartsData) {
    this._wg = wg;
    this._dmenu = dmenu;
    this._activity = activity;
    this._chartsData = chartsData;

    if (lcPath.length === 0) lcPath.push("summary");
    this._mSel = lcPath[0];
    this._lcPath = lcPath;

    this._serverWg = $("span")
      .text(server);
    this._activityWg = $("span");
    this.view();
    this.timer();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    let investing = 0;
    let assetsYesterday = 0;
    let assets = 0;
    this._chartsData.forEach(d => {
      d.accData.forEach(e => {
        investing += e.stocks * e.price;
        assetsYesterday += e.stocks * d.close;
        assets += e.stocks * d.quotes[0];
      });
    });
    const profits = assets - investing;
    const profitsToday = profits - assetsYesterday + investing;

    const wg = $("div");
    const dmenu = this._dmenu;
    const mSel = this._mSel;

    const lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("summary", _("Summary"), "daily"),
      Menu.separator(),
      Menu.tlink("portfolio", _("Portfolio"), "daily"),
      Menu.separator(),
      Menu.tlink("all", _("All CO's"), "daily"),
      Menu.separator(),
      Menu.tlink("sel", _("Selection"), "daily")
    ];
    function color (v) {
      return "color:" + (v > 0 ? "#00AAFF" : v < 0 ? "#FF8100" : "#404040");
    }
    const ropts = [
      new MenuEntry(Maybe.nothing, this._activityWg),
      Menu.separator2(),
      new MenuEntry(
        Maybe.nothing,
        $("span")
          .style(color(profitsToday))
          .text(new Dec(profitsToday, 2).toIso())
      ),
      Menu.separator(),
      new MenuEntry(
        Maybe.nothing,
        $("span")
          .style(color(profits))
          .text(new Dec(profits, 2).toIso())
      ),
      Menu.separator2(),
      new MenuEntry(Maybe.nothing, this._serverWg),
      Menu.separator(),
      Menu.toption(">>", ">>", () => this.newServer())
    ];
    dmenu.downMenu = new Menu(lopts, ropts, mSel);

    switch (mSel) {
    case "portfolio":
    case "all":
    case "sel":
      Cos.mk(wg, mSel, this._chartsData);
      break;
    default:
      new Summary(wg, this._chartsData); //eslint-disable-line
    }

    this._wg
      .removeAll()
      .add(wg)
    ;
  }

  /**
      @private
      @param {string} msg
  **/
  updateActivityWg (msg) {
    if (msg.length === 1) {
      const n = Number(msg);
      msg = "";
      for (let i = 0; i < n; ++i) {
        msg += " ·";
      }
    }
    this._activityWg
      .removeAll()
      .text(msg)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async newServer () {
    this._serverWg
      .removeAll()
      .add(Ui.img("wait.gif")
        .style("vertical-align:middle"))
    ;
    await Cts.client.send({
      "module": "daily",
      "source": "daily",
      "rq": "newServer"
    });
    Daily.mk(this._wg, this._dmenu, this._lcPath);
  }

  /**
      @private
  **/
  timer () {
    const self = this;
    let tm = null;
    const tmId = String(new Date().getTime());
    Store.put(ticIdKey, tmId);
    function fn () {
      if (tmId !== Store.take(ticIdKey)) {
        clearInterval(tm);
        return;
      }
      let previousAct = Store.take(actKey);
      if (previousAct === null) previousAct = "";
      const act = self._activity;
      const ticS = Store.take(ticKey);
      const tic = ticS === null ? 0 : Number(ticS);

      if (act === _("Active")) {
        self.updateActivityWg(String(4 - tic));
      } else {
        self.updateActivityWg(act);
      }

      if (tic < 4) {
        Store.put(ticKey, String(tic + 1));
      } else {
        Store.put(ticKey, "0");
        if (tm !== null) clearInterval(tm);
        Daily.mk(self._wg, self._dmenu, self._lcPath);
      }
    }

    fn();
    tm = setInterval(fn, 15000);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {!Dmenu} dmenu Double menu
      @param {!Array<string>} lcPath
      @return {!Promise<!Daily>}
  **/
  static async mk (wg, dmenu, lcPath) {
    const rp = await Cts.client.send({
      "module": "daily",
      "source": "daily",
      "rq": "idata"
    });
    const /** !Array<!DailyChart> */ chartsData =
      rp["chartsData"].map(e => DailyChart.fromJs(e));
    const /** string */ server = rp["server"];
    let /** string */ activity = rp["activity"];

    switch (activity) {
    case "Sleeping (1)": activity = _("Sleeping (1)"); break;
    case "Historic": activity = _("Historic"); break;
    case "Sleeping (2)": activity = _("Sleeping (2)"); break;
    case "Activating": activity = _("Activating"); break;
    case "Active": activity = _("Active"); break;
    default: activity = _("Deactivating");
    }
    return new Daily(wg, dmenu, lcPath, server, activity, chartsData);
  }

}

