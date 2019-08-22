// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import Domo from "../dmjs/Domo.js";  //eslint-disable-line
import DateDm from "../dmjs/DateDm.js";
import It from "../dmjs/It.js";
import DatePicker from "../dmjs/DatePicker.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Timetable from "../data/Timetable.js";
import MarketDay from "../data/MarketDay.js";
import Wrule from "../wgs/Wrule.js";
import Whour from "../wgs/Whour.js";

const $ = e => Ui.$(e);

function formatN (n) {
  if (n < 10) return "0" + String(n);
  return String(n);
}
/** Sys Schedule page. */
export default class Schedule {

  /**
   * @param {!SysMain} sysMain
   */
  constructor (sysMain) {
    this._sysMain = sysMain;

    // VIEW --------
    // TTTTTTTTTTTTT

    this._gopenDiv = $("div");
    this._gcloseDiv = $("div");
    this._gopen = null;
    this._gclose = null;

    this._holidaysDiv = $("div");

    this._specialDaysDiv = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  get general () {
    return $("table").att("align", "center").klass("frame")
      .add($("tr")
        .add($("td").klass("head").text(_("Open")))
        .add($("td").klass("head").text(_("CloseN"))))
      .add($("tr")
        .add($("td").add($("hr")))
        .add($("td").add($("hr"))))
      .add($("tr")
        .add($("td").add(this._gopenDiv))
        .add($("td").add(this._gcloseDiv)))
    ;
  }

  /**
   * @private
   * @param {!Array<string>} ls
   * @return {!Domo}
   */
  holidays (ls) {
    const self = this;
    function list () {
      if (ls.length === 0) {
        return [
          $("tr")
            .add($("td"))
            .add($("td").add($("table").att("align", "center").klass("frame4")
              .add($("tr").add($("td").html(_("Without dates"))))))
        ];
      }
      return ls.map(d => $("tr")
        .add($("td")
          .add(Ui.link(() => self.delHoliday(ls, d))
            .add(Ui.img("minus"))))
        .add($("td").style("text-align:center")
          .text(DateDm.fromStr(d).toString()))
      );
    }
    const dp = new DatePicker();
    const dpInput = $("input").att("type", "text")
      .style("text-align:center;width:166px");
    dp.action = (d) => {
      if (d === "") {
        dp.date = DateDm.now();
        dpInput.value(dp.date.toString());
      }
    };
    return $("table").att("align", "center").klass("frame")
      .add($("tr")
        .add($("td")
          .add(Ui.link(() => this.addHoliday(ls, dp.date.toBase()))
            .add(Ui.img("plus"))))
        .add($("td")
          .add(dp.makeText(dpInput))))
      .add($("tr").add($("td").att("colspan", 2).add($("hr"))))
      .adds(list())
    ;
  }

  /**
   * @private
   * @param {!Array<MarketDay>} ls
   * @return {!Domo}
   */
  specialDays (ls) {
    const self = this;
    function list () {
      if (ls.length === 0) {
        return [
          $("tr")
            .add($("td"))
            .add($("td").att("colspan", 3)
              .add($("table").att("align", "center").klass("frame4")
                .add($("tr").add($("td").html(_("Without dates"))))))
        ];
      }
      return ls.map(e => $("tr")
        .add($("td")
          .add(Ui.link(() => self.delSpecialDay(ls, e.date))
            .add(Ui.img("minus"))))
        .add($("td").style("text-align:center")
          .text(DateDm.fromStr(e.date).toString()))
        .add($("td").style("text-align:center")
          .text(`${formatN(e.hopen)}:${formatN(e.mopen)}`))
        .add($("td").style("text-align:center")
          .text(`${formatN(e.hclose)}:${formatN(e.mclose)}`))
      );
    }
    const dp = new DatePicker();
    const dpInput = $("input").att("type", "text")
      .style("text-align:center;width:166px");
    dp.action = (d) => {
      if (d === "") {
        dp.date = DateDm.now();
        dpInput.value(dp.date.toString());
      }
    };
    const open = new Whour(this._gopen.hour, this._gopen.minute, () => {});
    const close = new Whour(this._gclose.hour, this._gclose.minute, () => {});
    return $("table").att("align", "center").klass("frame")
      .add($("tr")
        .add($("td")
          .add(Ui.link(() =>
            this.addSpecialDay(
              ls, dp.date.toBase(),
              open.hour, open.minute, close.hour, close.minute
            )
          ).add(Ui.img("plus"))))
        .add($("td")
          .add(dp.makeText(dpInput)))
        .add($("td").add(open.wg.klass("frame")))
        .add($("td").add(close.wg.klass("frame"))))
      .add($("tr").add($("td").att("colspan", 4).add($("hr"))))
      .add($("tr")
        .add($("td"))
        .add($("td").klass("head").text(_("Date")))
        .add($("td").klass("head").text(_("Open")))
        .add($("td").klass("head").text(_("CloseN"))))
      .add($("tr").add($("td").att("colspan", 4).add($("hr"))))
      .adds(list())
    ;
  }

  /** @return {void} */
  show () {
    this._sysMain.view.removeAll().add(
      $("div").style("text-align:center")
        .add($("div").klass("head").text(_("Schedule")))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td").att("colspan", 2)
              .add(Wrule.mkBig(_("General Time Table")))))
          .add($("tr")
            .add($("td").att("colspan", 2).add(this.general)))
          .add($("tr")
            .add($("td").style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Holidays")))
              .add(this._holidaysDiv))
            .add($("td").style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Special days")))
              .add(this._specialDaysDiv))))
    );

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @return {!Promise} */
  async update () {
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "general"
    };
    const rp = await Main.client.send(rq);

    /** @type {!Timetable} */
    const g = Timetable.fromJs(rp["general"]);

    this._gopen = new Whour(
      g.hopen, g.mopen, this.generalChange.bind(this)
    );
    this._gopenDiv.removeAll().add(this._gopen.wg.klass("frame"));
    this._gclose = new Whour(
      g.hclose, g.mclose, this.generalChange.bind(this)
    );
    this._gcloseDiv.removeAll().add(this._gclose.wg.klass("frame"));

    this.updateHolidays();

    this.updateSpecialDays();
  }

  /** @private */
  async updateHolidays () {
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "holidays"
    };
    const rp = await Main.client.send(rq);
    /** @type {!Array<string>} */
    const ls = rp["holidays"].sort();

    this._holidaysDiv.removeAll().add(this.holidays(ls));
  }

  /** @private */
  async updateSpecialDays () {
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "specialDays"
    };
    const rp = await Main.client.send(rq);
    /** @type {!Array<MarketDay>} */
    const ls = rp["specialDays"]
      .map(e => MarketDay.fromJs(e))
      .sort((e1, e2) => e1.date > e2.date ? 1 : -1);

    this._specialDaysDiv.removeAll().add(this.specialDays(ls));
  }

  /** @private */
  generalChange () {
    const tt = new Timetable(
      this._gopen.hour, this._gopen.minute,
      this._gclose.hour, this._gclose.minute
    );
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "setGeneral",
      "timetable": tt.toJs()
    };
    Main.client.send(rq);
  }

  /**
   * @private
   * @param {!Array<string>} ls
   * @param {string} d
   * @return {void}
   */
  addHoliday (ls, d) {
    const now = DateDm.now().toBase();
    if (d < now) {
      alert(_("Date before today"));
      return;
    }
    if (It.from(ls).contains(d)) {
      alert(_args(_("Date '%0' is duplicated"), d));
      return;
    }
    ls.push(d);
    this.holidaysChange(ls);
  }

  /**
   * @private
   * @param {!Array<string>} ls
   * @param {string} d
   * @return {void}
   */
  delHoliday (ls, d) {
    if (confirm(_args(_("Delete '%0'?"), d))) {
      this.holidaysChange(ls.filter(e => e !== d));
    }
  }

  /** @private */
  async holidaysChange (ls) {
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "setHolidays",
      "holidays": ls
    };
    await Main.client.send(rq);
    this.updateHolidays();
  }

  /**
   * @private
   * @param {!Array<MarketDay>} ls
   * @param {string} date
   * @param {number} hopen
   * @param {number} mopen
   * @param {number} hclose
   * @param {number} mclose
   * @return {void}
   */
  addSpecialDay (ls, date, hopen, mopen, hclose, mclose) {
    const now = DateDm.now().toBase();
    if (date < now) {
      alert(_("Date before today"));
      return;
    }
    if (It.from(ls).indexf(e => e.date === date) !== -1) {
      alert(_args(_("Date '%0' is duplicated"), date));
      return;
    }
    ls.push(new MarketDay(date, hopen, mopen, hclose, mclose));
    this.specialDaysChange(ls);
  }

  /**
   * @private
   * @param {!Array<MarketDay>} ls
   * @param {string} d
   * @return {void}
   */
  delSpecialDay (ls, d) {
    if (confirm(_args(_("Delete '%0'?"), d))) {
      this.specialDaysChange(ls.filter(e => e.date !== d));
    }
  }

  /** @private */
  async specialDaysChange (ls) {
    const rq = {
      "module": "sys",
      "source": "Schedule",
      "rq": "setSpecialDays",
      "specialDays": ls.map(e => e.toJs())
    };
    await Main.client.send(rq);
    this.updateSpecialDays();
  }

}
