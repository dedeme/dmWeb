// Copyright 13-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";
import {_} from "../../I18n.js";
import Main from "../../Main.js";

const $ = e => Ui.$(e);

/** Fleas log launched in Home page */
export default class Flog {
  constructor () {

    // MODEL -------
    // TTTTTTTTTTTTT

    this._auto = false;
    this._interval = 0;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._Lmenu = $("div");
    this._Rmenu = $("div");
    this._area = $("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25).att("cols", 85);

    // CONTROL -----
    // TTTTTTTTTTTTT
    this.update();
  }

  /** @return {number} */
  get interval () {
    return this._interval;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  mkEntry (sel, text, action) {
    if (sel) {
      return $("span").klass("frame").text(text);
    }

    return Ui.link(action.bind(this)).klass("link").text(text);
  }

  get wg () {
    return $("div")
      .add($("div").klass("head").text(_("Fleas Log")))
      .add($("table").att("align", "center").klass("frame3")
        .add($("tr")
          .add($("td").style("text-align:left")
            .add(this._Lmenu))
          .add($("td").style("text-align:right")
            .add(this._Rmenu)))
        .add($("tr").add($("td").att("colspan", 2)))
        .add($("tr")
          .add($("td").att("colspan", 2).add(this._area))))
    ;
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  async reload () {
    const rq = {
      "module": "sys",
      "source": "Home",
      "rq": "logFleas"
    };
    const rp = await Main.client.send(rq);
    this._area.value(rp["log"].join("\n"));
  }

  /** @private */
  mkMenu () {
    if (this._auto) {
      this._Lmenu.removeAll().add(
        Ui.link(this.manual.bind(this)).klass("link").text(_("Manual"))
      );
      this._Rmenu.removeAll().add($("span").text(_("Auto")));
    } else {
      this._Lmenu.removeAll().add(
        Ui.link(this.auto.bind(this)).klass("link").text(_("Auto"))
      );
      this._Rmenu.removeAll().add(
        Ui.link(this.reload.bind(this)).klass("link").text(_("Reload"))
      );
    }
  }

  /**
   * @private
   * @return {void}
   */
  update () {
    this.mkMenu();
    this.reload();
  }

  /** @private */
  manual () {
    this._auto = false;
    clearInterval(this._interval);
    this.update();
  }

  /** @private */
  auto () {
    this._auto = true;
    this._interval = setInterval(this.reload.bind(this), 1000);
    this.update();
  }

  /**
   * Show fleas log in a new window
   * @return {void}
   */
  static open () {
    const log = new Flog();
    const w = window.open("", _("Log"),
      "menubar=0,toolbar=0,location=0,personalbar=0,status=0," +
      "width=700,height=500"
    );
    w.document.write(
      "<head>" +
      " <title>MultiMarket</title>" +
      " <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />" +
      " <meta http-equiv='Pragma' content='no-cache' />" +
      " <meta name='lang' content='es' />" +
      " <meta name='author' content='ºDeme.' />" +
      " <link rel='stylesheet' href='styles.css' type='text/css' />" +
      "</head>" +
      "<body>" +
      "</body>" +
      "</html>"
    );
    new Domo(w.document.querySelector("body"))
      .add(log.wg)
      .add($("div").style("text-align:center;padding-top:8px")
        .add($("button").text(_("Close")).on("click", () => {
          clearInterval(log.interval);
          w.close();
        })))
    ;
    log.reload();
  }

}
