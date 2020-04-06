// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../../dmjs/Ui.js";
import Domo from "../../dmjs/Domo.js";  //eslint-disable-line
import Client from "../../dmjs/Client.js"; //eslint-disable-line
import DateDm from "../../dmjs/DateDm.js";
import It from "../../dmjs/It.js";
import DatePicker from "../../dmjs/DatePicker.js";
import {_, _args} from "../../I18n.js";
import Timetable from "../../data/Timetable.js";
import MarketDay from "../../data/MarketDay.js";
import Wrule from "../../wgs/Wrule.js";
import Whour from "../../wgs/Whour.js";

const $ = e => Ui.$(e);

function formatN (n) {
  if (n < 10) return "0" + String(n);
  return String(n);
}
/** Sys Calendar page. */
export default class Calendar {

  /**
      @param {!Client} client
      @param {string} timeStamp
      @param {!Timetable} general
      @param {!Array<string>} holidays
      @param {!Array<!MarketDay>} specialDays
  **/
  constructor (client, timeStamp, general, holidays, specialDays) {
    this._client = client;
    this._timeStamp = timeStamp;
    this._general = general;
    this._holidays = holidays;
    this._specialDays = specialDays;


    this._gopen = new Whour(
      general.hopen, general.mopen, () => { this.generalChange() }
    );
    this._gclose = new Whour(
      general.hclose, general.mclose, () => { this.generalChange() }
    );

    this._generalDiv = $("div").style("padding-bottom:15px");
    this._holidaysDiv = $("div");
    this._specialDaysDiv = $("div");
    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // view ----------------------------------------------------------------------

  /**
      @private
  **/
  general () {
    this._generalDiv.removeAll()
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td").klass("head").text(_("Open")))
          .add($("td").klass("head").text(_("CloseN"))))
        .add($("tr")
          .add($("td").add($("hr")))
          .add($("td").add($("hr"))))
        .add($("tr")
          .add($("td").add(this._gopen.wg.klass("frame")))
          .add($("td").add(this._gclose.wg.klass("frame")))))
    ;
  }

  /**
      @private
  **/
  holidays () {
    const self = this;
    const ls = this._holidays.sort((e1, e2) => e1 > e2 ? 1 : -1);

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
          .add(Ui.link(() => { self.delHoliday(ls, d) })
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
    this._holidaysDiv.removeAll()
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => { this.addHoliday(ls, dp.date.toBase()) })
              .add(Ui.img("plus"))))
          .add($("td")
            .add(dp.makeText(dpInput))))
        .add($("tr").add($("td").att("colspan", 2).add($("hr"))))
        .adds(list()))
    ;
  }

  /**
      @private
  **/
  specialDays () {
    const self = this;
    const ls = this._specialDays.sort((e1, e2) => e1.date > e2.date ? 1 : -1);

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
          .add(Ui.link(() => { self.delSpecialDay(ls, e.date) })
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
    this._specialDaysDiv.removeAll()
      .add($("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => {
              this.addSpecialDay(
                ls, dp.date.toBase(),
                open.hour, open.minute, close.hour, close.minute
              );
            }).add(Ui.img("plus"))))
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
        .adds(list()))
    ;
  }

  /**
      @private
  **/
  view () {
    this.general();
    this.holidays();
    this.specialDays();
    this._wg.removeAll().add(
      $("div").style("text-align:center")
        .add($("div").klass("head").text(_("Calendar")))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td").att("colspan", 2)
              .add(Wrule.mkBig(_("General Time Table")))))
          .add($("tr")
            .add($("td").att("colspan", 2).add(this._generalDiv)))
          .add($("tr")
            .add($("td").style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Holidays")))
              .add(this._holidaysDiv))
            .add($("td").style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Special days")))
              .add(this._specialDaysDiv))))
    );
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return {!Promise<void>}
  **/
  async generalChange () {
    const tt = new Timetable(
      this._gopen.hour, this._gopen.minute,
      this._gclose.hour, this._gclose.minute
    );
    await this._client.ssend({
      "module": "Settings",
      "source": "Calendar",
      "rq": "setGeneral",
      "timeTable": tt.toJs()
    });
  }

  /**
      @private
      @return {!Promise<void>}
  **/
  async holidaysChange (ls) {
    await this._client.ssend({
      "module": "Settings",
      "source": "Calendar",
      "rq": "setHolidays",
      "holidays": ls
    });
    this._holidays = ls;
    this.holidays();
  }

  /**
      @private
      @param {!Array<string>} ls
      @param {string} d
      @return {void}
  **/
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
      @private
      @param {!Array<string>} ls
      @param {string} d
      @return {void}
  **/
  delHoliday (ls, d) {
    if (confirm(_args(_("Delete '%0'?"), d))) {
      this.holidaysChange(ls.filter(e => e !== d));
    }
  }

  /**
      @private
      @return {!Promise<void>}
  **/
  async specialDaysChange (ls) {
    await this._client.ssend({
      "module": "Settings",
      "source": "Calendar",
      "rq": "setSpecialDays",
      "specialDays": ls.map(e => e.toJs())
    });
    this._specialDays = ls;
    this.specialDays();
  }

  /**
      @private
      @param {!Array<MarketDay>} ls
      @param {string} date
      @param {number} hopen
      @param {number} mopen
      @param {number} hclose
      @param {number} mclose
      @return {void}
  **/
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
      @private
      @param {!Array<MarketDay>} ls
      @param {string} d
      @return {void}
  **/
  delSpecialDay (ls, d) {
    if (confirm(_args(_("Delete '%0'?"), d))) {
      this.specialDaysChange(ls.filter(e => e.date !== d));
    }
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @return {!Promise<!Calendar>}
  **/
  static async mk (client) {
    const rp = await client.ssend({
      "module": "Settings",
      "source": "Calendar",
      "rq": "idata"
    });
    const timeStamp = rp["timeStamp"];
    const general = Timetable.fromJs(rp["general"]);
    const holidays = rp["holidays"];
    const specialDays = rp["specialDays"].map(a => MarketDay.fromJs(a));
    return new Calendar(client, timeStamp, general, holidays, specialDays);
  }


}
