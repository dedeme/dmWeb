// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line
import DateDm from "../dmjs/DateDm.js";
import {_} from "../I18n.js";
import LogEntry from "../data/LogEntry.js";

const $ = e => Ui.$(e);

/**
    Log widget.
**/
export default class Log {
  /**
      @param {boolean} is2Days
      @param {boolean} isErrors
      @param {!Array<!LogEntry>} log
      @param {function ():!Promise<!Array<!LogEntry>>} load
      @param {function ():!Promise<void>} reset
  **/
  constructor (is2Days, isErrors, log, load, reset) {
    this._is2Days = is2Days;
    this._isErrors = isErrors;
    this._log = log;
    this._load = load;
    this._reset = reset;

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @return {void}
  **/
  view () {
    function mkOption (isImg, isSel, id, action) {
      if (isImg) {
        const r = Ui.link(() => { action() });
        if (isSel) r.klass("frame");
        return r.add(Ui.img(id).style("vertical-align:top"));
      }
      let r = $("span").klass("frame");
      if (!isSel) r = Ui.link(() => { action() }).klass("link");
      return r.text(id);


    }

    const lmenu = $("div");
    const rmenu = $("div");
    const area = $("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25).att("cols", LogEntry.lineWidth() + 5);

    lmenu
      .add($("span")
        .add(mkOption(
          false, this._is2Days, _("2 Days"), () => { this.on2Days() }
        )))
      .add($("span").html(" · "))
      .add($("span")
        .add(mkOption(
          false, !this._is2Days, _("All"), () => { this.onAllD() }
        )))
    ;

    rmenu
      .add($("span")
        .add(mkOption(
          false, false, _("Reload"), () => { this.onReload() }
        )))
      .add($("span").html(" · "))
      .add($("span")
        .add(mkOption(
          false, false, _("Delete"), () => { this.onDelete() }
        )))
      .add($("span").html(" | "))
      .add($("span")
        .add(mkOption(
          false, this._isErrors, _("Errors"), () => { this.onErrors() }
        )))
      .add($("span").html(" · "))
      .add($("span")
        .add(mkOption(
          false, !this._isErrors, _("All"), () => { this.onAll() }
        )))
    ;

    const today = DateDm.now();
    area.value(
      this._log
        .filter(e =>
          (this._is2Days ? today.df(e.date) < 3 : true) &&
          (this._isErrors ? e.isError : true)
        )
        .map(e => e.toString())
        .join("\n")
    );

    this._wg.removeAll()
      .add($("div").klass("head").style("padding-bottom:10px").text(_("Log")))
      .add($("table").att("align", "center").klass("frame3")
        .add($("tr")
          .add($("td").style("text-align:left")
            .add(lmenu))
          .add($("td").style("text-align:right")
            .add(rmenu)))
        .add($("tr").add($("td").att("colspan", 2)))
        .add($("tr")
          .add($("td").att("colspan", 2).add(area))))
    ;
  }

  // Control -------------------------------------------------------------------

  on2Days () {
    this._is2Days = true;
    this.view();
  }

  onAllD () {
    this._is2Days = false;
    this.view();
  }

  async onReload () {
    this._log = await this._load();
    this.view();
  }

  async onDelete () {
    if (confirm(_("All log entries will be deleted.\nContinue?"))) {
      await this._reset();
      this._log = [];
      this.view();
    }
  }

  onErrors () {
    this._isErrors = true;
    this.view();
  }

  onAll () {
    this._isErrors = false;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {boolean} is2Days
      @param {boolean} isErrors
      @param {function ():!Promise<!Array<!LogEntry>>} load
      @param {function ():!Promise<void>} reset
      @return {!Promise<!Log>}
  **/
  static async mk (is2Days, isErrors, load, reset) {
    const log = await load();
    return new Log(is2Days, isErrors, log, load, reset);
  }

}
