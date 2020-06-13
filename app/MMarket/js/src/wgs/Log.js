// Copyright 23-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js"; // eslint-disable-line
import DateDm from "../dmjs/DateDm.js";
import {_} from "../I18n.js";
import LogRow from "../data/LogRow.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Log widget.
**/
export default class Log {
  /**
      @param {!Domo} wg
      @param {!Array<!LogRow>} log
      @param {function ():!Promise<!Array<!LogRow>>} load
      @param {function ():!Promise<void>} reset
  **/
  constructor (wg, log, load, reset) {
    this._is2Days = true;
    this._isErrors = true;
    this._lineWidth = 120;
    this._wg = wg;
    this._log = log;
    this._load = load;
    this._reset = reset;

    this.view();
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
      .att("rows", 25).att("cols", this._lineWidth + 5);

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
        .map(e => e.format(this._lineWidth))
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

  /**
      @private
      @return void
  **/
  on2Days () {
    this._is2Days = true;
    this.view();
  }

  /**
      @private
      @return void
  **/
  onAllD () {
    this._is2Days = false;
    this.view();
  }

  /**
      @private
      @return void
  **/
  async onReload () {
    this._log = await this._load();
    this.view();
  }

  /**
      @private
      @return void
  **/
  async onDelete () {
    if (confirm(_("All log entries will be deleted.\nContinue?"))) {
      await this._reset();
      this._log = [];
      this.view();
    }
  }

  /**
      @private
      @return void
  **/
  onErrors () {
    this._isErrors = true;
    this.view();
  }

  /**
      @private
      @return void
  **/
  onAll () {
    this._isErrors = false;
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {function ():!Promise<!Array<!LogRow>>} load
      @param {function ():!Promise<void>} reset
      @return {!Promise<!Log>}
  **/
  static async mk (wg, load, reset) {
    const log = await load();
    return new Log(wg, log, load, reset);
  }

}
