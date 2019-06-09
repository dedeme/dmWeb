// Copyright 24-Mar-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../dmjs/Ui.js";
import DatePicker from "../dmjs/DatePicker.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_} from "../I18n.js";
import Timetable from "../data/Timetable.js";
import MarketDay from "../data/MarketDay.js";
import Wrule from "../wgs/Wrule.js";
import Whour from "../wgs/Whour.js";

const $ = Ui.$;

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

    this._hDatePicker = new DatePicker();

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

  /** @private */
  get holidays () {
    return $("table").att("align", "center").klass("frame")
      .add($("tr")
        .add($("td")
          .add(Ui.link(this.addHoliday.bind(this)).add(Ui.img("plus"))))
        .add($("td")
          .add(this._hDatePicker.makeText(
            $("input").att("type", "text").style("width:166px")
          ))))
      .add($("tr").add($("td").att("colspan", 2).add($("hr"))))
    ;
  }

  /** @private */
  get specialDays () {
    return $("table").html("specialDays");
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
              .add(this.holidays))
            .add($("td").style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Special days")))
              .add(this.specialDays))))
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
      "rq": "idata"
    };
    const rp = await Main.client.send(rq);

    /** @type {!Timetable} */
    const g = Timetable.fromJs(rp["general"]);
    /** @type {!Array<string>} */
    const hs = rp["holidays"];
    /** @type {!Array<MarketDay>} */
    const sds = rp["specialDays"];

    this._gopen = new Whour(
      g.hopen, g.mopen, this.generalChange.bind(this)
    );
    this._gopenDiv.removeAll().add(this._gopen.wg.klass("frame"));
    this._gclose = new Whour(
      g.hclose, g.mclose, this.generalChange.bind(this)
    );
    this._gcloseDiv.removeAll().add(this._gclose.wg.klass("frame"));
  }

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

  addHoliday () {
    alert("add holiday");
  }

}
