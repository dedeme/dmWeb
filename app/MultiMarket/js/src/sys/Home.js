// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Log from "./home/Log.js";
import Flog from "./home/Flog.js";
import Wrule from "../wgs/Wrule.js";

const $ = Ui.$;

/** Sys Home page. */
export default class Home {

  /**
   * @param {!SysMain} sysMain
   */
  constructor (sysMain) {
    this._sysMain = sysMain;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._logDiv = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {void} */
  show () {
    this._sysMain.view.removeAll().add(
      $("div").style("text-align:center")
        .add($("div").klass("head").text(_("Home")))
        .add($("table").klass("frame").att("align", "center")
          .add($("tr").add($("td").style("text-align:left")
            .add($("a").att("href", Main.urlBase + "?market")
              .text(_("Market")))
            .add($("br"))
            .add($("a").att("href", Main.urlBase + "?daily")
              .text(_("Daily Quotes")))
            .add($("br"))
            .add($("a").att("href", Main.urlBase + "?fleas")
              .text(_("Fleas Data"))))))
        .add($("div").klass("head").text(_("Management")))
        .add($("div")
          .add($("table").att("align", "center")
            .add($("tr")
              .add($("td").klass("frame").style("text-align:center")
                .add(Wrule.mkSmall(_("Log")))
                .add(Ui.link(this.clearLog.bind(this))
                  .klass("link").text(_("Clear"))))
              .add($("td").klass("frame").style("text-align:center")
                .add(Wrule.mkSmall(_("Fleas")))
                .add(Ui.link(this.runFleas.bind(this))
                  .klass("link").text(_("Run")))
                .add($("span").html("&nbsp;·&nbsp;"))
                .add(Ui.link(this.stopFleas.bind(this))
                  .klass("link").text(_("Stop")))
                .add($("span").html("&nbsp;|&nbsp;"))
                .add(Ui.link(this.logFleas.bind(this))
                  .klass("link").text(_("Log")))))))
        .add(this._logDiv)
        .add(Ui.upTop("up"))
    );

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "sys",
      "source": "Home",
      "rq": "getLog"
    };
    const rp = await Main.client.send(rq);
    const log = rp["log"];

    this._logDiv.removeAll().add(new Log(log).wg); //eslint-disable-line
  }

  /** @private */
  async clearLog () {
    if (confirm(_("All log entries will be deleted.\nContinue?"))) {
      const rq = {
        "module": "sys",
        "source": "Home",
        "rq": "clearLog"
      };
      await Main.client.send(rq);

      this._logDiv.removeAll().add(new Log([]).wg); //eslint-disable-line
    }
  }

  /** @private */
  runFleas () {
    const rq = {
      "module": "sys",
      "source": "Home",
      "rq": "runFleas"
    };
    Main.client.send(rq);
    alert(_("Fleas running"));
  }

  /** @private */
  async stopFleas () {
    if (confirm(_("End fleas running?"))) {
      const rq = {
        "module": "sys",
        "source": "Home",
        "rq": "stopFleas"
      };
      await Main.client.send(rq);
      alert(_("Fleas stopped"));
    }
  }

  /** @private */
  logFleas () {
    Flog.open();
  }
}
