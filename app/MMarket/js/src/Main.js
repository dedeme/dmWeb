// Copyright 21-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Dmenu from "./wgs/Dmenu.js";
import Cts from "./data/Cts.js";
import Authentication from "./Authentication.js";
import {I18n, _} from "./I18n.js";
import Msg from "./wgs/Msg.js";
import Home from "./pgs/home/Home.js";
import Fleas from "./pgs/fleas/Fleas.js";
import Ranking from "./pgs/ranking/Ranking.js";
import Daily from "./pgs/daily/Daily.js";
import Acc from "./pgs/acc/Acc.js";
import Settings from "./pgs/settings/Settings.js";

const $ = e => Ui.$(e);

/**
    Application entry2.
**/
export default class Main {

  /**
      @param {!Domo} wg
      @param {string} lang
  **/
  constructor (wg, lang) {
    this._wg = wg;
    this._lang = lang;
    this._afterShow = () => {};

    const search = location.search;
    const lcPath = search === ""
      ? []
      : search.substring(1).split("&")
    ;
    if (lcPath.length === 0) lcPath.push("home");
    this._lcPath = lcPath;

    switch (lcPath[0]) {
    case "fleas":
    case "ranking":
    case "daily":
    case "acc":
    case "settings":
      this._target = lcPath[0];
      break;
    default:
      this._target = "home";
    }

    this.view();
  }


  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    const tg = this._target;
    const menuDiv = $("div");
    const menu = new Dmenu(menuDiv, tg);
    const lcPath = this._lcPath;
    lcPath.shift();

    const bodyDiv = $("div");
    switch (tg) {
    case "home":
      Home.mk(bodyDiv);
      break;
    case "fleas":
      Fleas.mk(bodyDiv, menu, lcPath);
      break;
    case "daily":
      Daily.mk(bodyDiv, menu, lcPath);
      break;
    case "ranking":
      Ranking.mk(bodyDiv);
      break;
    case "acc":
      new Acc(bodyDiv, menu, lcPath); //eslint-disable-line
      break;
    case "settings":
      new Settings(bodyDiv, menu, lcPath, this._lang); //eslint-disable-line
      break;
    default:
      Home.mk(bodyDiv);
    }

    this._wg
      .removeAll()
      .add(menuDiv)
      .add(bodyDiv)
      .add($("p").html("&nbsp;"))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add($("a")
              .att("href", "doc/about.html")
              .att("target", "blank")
              .html("<small>" + _("Help & Credits") + "</small>")))
          .add($("td")
            .style(`text-align: right;font-size: 10px;
                    color:#808080;font-size:x-small;`)
            .html(`- © ºDeme. ${Cts.appName} (${Cts.appVersion}) -`))))
      .add(Ui.upTop("up"))
      .add(Msg.wg)
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      Action to do after loading page.
      @return void
  **/
  afterShow () {
    this._afterShow();
  }

  // Static --------------------------------------------------------------------

  /**
      Creates a new application main page or send to authentication if
      connection is not possible.
      @param {!Domo} wg
      @return !Promise<!Main | !Athentication>
  **/
  static async mk (wg) {
    const /** boolean */ ok = await Cts.client.connect();
    if (ok) {
      const rp = await Cts.client.send({
        "module": "main",
        "source": "main",
        "rq": "lang"
      });
      const lang = rp["lang"];

      if (lang === "en")
        I18n.en();
      else
        I18n.es();

      return new Main(wg, lang);
    }
    return new Authentication(wg, Cts.appName, () => { Main.mk(wg) });
  }

  /**
      Start application.
      @return !Promise<void>
  **/
  static async run () {
    const wg = $("div");
    const main = await Main.mk(wg);
    $("@body")
      .removeAll()
      .add(wg);
    main.afterShow();
  }
}
