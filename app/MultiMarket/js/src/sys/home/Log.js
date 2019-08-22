// Copyright 13-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../dmjs/Domo.js";
import Ui from "../../dmjs/Ui.js";
import DateDm from "../../dmjs/DateDm.js";
import {_} from "../../I18n.js";
import Main from "../../Main.js";

const $ = e => Ui.$(e);

/** Log used in Home page */
export default class Log {
  /** @param {!Array<string>} text */
  constructor (text) {

    // MODEL -------
    // TTTTTTTTTTTTT

    this._text = text;
    this._day = true;
    this._week = false;
    this._ever = false;
    this._error = true;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._Lmenu = $("div");
    this._Rmenu = $("div");
    this._area = $("textarea").att("spellcheck", false)
      .att("rows", 25).att("cols", 85);

    // CONTROL -----
    // TTTTTTTTTTTTT
    this.update();
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
      .add($("div").klass("head").text(_("Log")))
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

  onLastDay () {
    this._day = true;
    this._week = false;
    this._ever = false;
    this.update();
  }

  onLastWeek () {
    this._day = false;
    this._week = true;
    this._ever = false;
    this.update();
  }

  onEver () {
    this._day = false;
    this._week = false;
    this._ever = true;
    this.update();
  }

  onReload () {
    this.reload();
  }

  onError () {
    this._error = true;
    this.update();
  }

  onAll () {
    this._error = false;
    this.update();
  }

  onWindow () {
    Log.open();
  }

  /**
   * @private
   */
  setText () {
    const now = DateDm.now();
    const now1 = now.add(-1);
    const now7 = now.add(-7);

    this._area.value(this._text
      .filter(e => this._error ? e.charAt(0) === "E" : true)
      .filter(e => {
        const d = DateDm.fromIso(e.substring(1, 11));
        if (this._day) {
          return d.eq(now) || d.eq(now1);
        }
        if (this._week) {
          return d.compare(now7) >= 0;
        }
        return true;
      }).map(e => e.substring(1))
      .reduce((r, e) => r += e + "\n", "")
    );
  }

  /**
   * @private
   * @return {void}
   */
  update () {
    const leftMenu = $("div")
      .add($("span")
        .add(this.mkEntry(this._day, _("Last Day"), this.onLastDay)))
      .add($("span").html(" · "))
      .add($("span")
        .add(this.mkEntry(this._week, _("Last Week"), this.onLastWeek)))
      .add($("span").html(" · "))
      .add($("span")
        .add(this.mkEntry(this._ever, _("All"), this.onEver)))
    ;

    const rightMenu = $("div")
      .add($("span")
        .add(this.mkEntry(false, _("Reload"), this.onReload)))
      .add($("span").html(" | "))
      .add($("span")
        .add(this.mkEntry(this._error, _("Errors"), this.onError)))
      .add($("span").html(" · "))
      .add($("span")
        .add(this.mkEntry(!this._error, _("All"), this.onAll)))
    ;
    if (this._text.length > 0) {
      rightMenu.add($("span").html(" | "))
        .add($("span")
          .add(Ui.link(this.onWindow.bind(this))
            .add(Ui.img("window-new"))).style("vertical-align:top"))
      ;
    }

    this._Lmenu.removeAll().add(leftMenu);
    this._Rmenu.removeAll().add(rightMenu);

    this.setText();
  }

  /** @private */
  async reload () {
    const rq = {
      "module": "sys", // Reuse callback
      "source": "Home",
      "rq": "getLog"
    };
    const rp = await Main.client.send(rq);
    const log = rp["log"];
    this._text = log;
    this.setText();
  }

  /**
   * Show log in a new window
   * @return {void}
   */
  static open () {
    const log = new Log([]);
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
        .add($("button").text(_("Close")).on("click", () => w.close())))
    ;
    log.reload();
  }

}
