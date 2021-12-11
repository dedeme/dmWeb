// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.calendarPg;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dt;
import dm.Opt;
import dm.It;
import dm.DatePicker;
import pgs.settings.calendarPg.wgs.Whour;
import data.calendar.Calendar;
import data.calendar.Timetable;
import data.calendar.MarketDay;
import wgs.Wrule;
import wgs.Msg;
import data.Cts;
import I18n._;
import I18n._args;

/// Page to set market calendar.
class CalendarPg {
  final wg: Domo;
  var calendar: Calendar;
  var gopen: Whour;
  var gclose: Whour;
  final generalDiv = Q("div")
    .style("padding-bottom:15px")
  ;
  final holidaysDiv = Q("div");
  final specialDaysDiv = Q("div");


  function new (wg: Domo, calendar: Calendar) {
    this.wg = wg;
    this.calendar = calendar;

    gopen = new Whour(
      calendar.general.hopen, calendar.general.mopen, generalChange
    );
    gclose = new Whour(
      calendar.general.hclose, calendar.general.mclose, this.generalChange
    );

    view();
  }

  // view ----------------------------------------------------------------------

  function generalWg () {
    generalDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .klass("head")
            .text(_("Open")))
          .add(Q("td")
            .klass("head")
            .text(_("CloseN"))))
        .add(Q("tr")
          .add(Q("td").style("padding:0px")
            .add(Q("hr")))
          .add(Q("td").style("padding:0px")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .add(gopen.wg.klass("frame")))
          .add(Q("td")
            .add(gclose.wg.klass("frame")))))
    ;
  }

  function holidaysWg () {
    final ls = calendar.holidays.copy();
    ls.sort((e1, e2) -> e1 > e2 ? 1 : -1);

    function list () {
      if (ls.length == 0) {
        return [
          Q("tr")
            .add(Q("td"))
            .add(Q("td")
              .add(Q("table")
                .att("align", "center")
                .klass("frame4")
                .add(Q("tr")
                  .add(Q("td")
                    .html(_("Without dates"))))))
        ];
      }
      return ls.map(d -> Q("tr")
        .add(Q("td")
          .add(Ui.link(e -> delHoliday(ls, d))
            .add(Ui.img("minus"))))
        .add(Q("td")
          .style("text-align:center")
          .text(Dt.toIso(Opt.eget(Dt.from(d)))))
      );
    }
    final dp = new DatePicker();
    final dpInput = Q("input")
      .att("type", "text")
      .style("text-align:center;width:166px")
    ;
    dp.action = d -> {
      if (d == "") {
        dp.date = Date.now();
        dpInput.value(dp.date.toString());
      }
    };
    holidaysDiv
      .removeAll()
      .add(Q("table").att("align", "center").klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(e -> addHoliday(ls, Dt.to(dp.date)))
              .add(Ui.img("plus"))))
          .add(Q("td")
            .add(dp.mkText(dpInput))))
        .add(Q("tr")
          .add(Q("td").att("colspan", 2)
            .add(Q("hr"))))
        .adds(list()))
    ;
  }

  function specialDaysWg () {
    final ls = calendar.specialDays.copy();
    ls.sort((e1, e2) -> e1.date > e2.date ? 1 : -1);

    function list () {
      if (ls.length == 0) {
        return [
          Q("tr")
            .add(Q("td"))
            .add(Q("td")
              .att("colspan", 3)
              .add(Q("table")
                .att("align", "center")
                .klass("frame4")
                .add(Q("tr")
                  .add(Q("td")
                    .html(_("Without dates"))))))
        ];
      }
      return ls.map(e -> Q("tr")
        .add(Q("td")
          .add(Ui.link(ev -> delSpecialDay(ls, e.date))
            .add(Ui.img("minus"))))
        .add(Q("td")
          .style("text-align:center")
          .text(Dt.toIso(Opt.eget(Dt.from(e.date)))))
        .add(Q("td")
          .style("text-align:center")
          .text('${Fns.format00(e.hopen)}:${Fns.format00(e.mopen)}'))
        .add(Q("td")
          .style("text-align:center")
          .text('${Fns.format00(e.hclose)}:${Fns.format00(e.mclose)}'))
      );
    }
    final dp = new DatePicker();
    final dpInput = Q("input")
      .att("type", "text")
      .style("text-align:center;width:166px");
    dp.action = d -> {
      if (d == "") {
        dp.date = Date.now();
        dpInput.value(dp.date.toString());
      }
    };
    final open = new Whour(gopen.hour, gopen.minute, () -> {});
    final close = new Whour(gclose.hour, gclose.minute, () -> {});
    specialDaysDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(ev -> {
              addSpecialDay(
                ls, Dt.to(dp.date),
                open.hour, open.minute, close.hour, close.minute
              );
            }).add(Ui.img("plus"))))
          .add(Q("td")
            .add(dp.mkText(dpInput)))
          .add(Q("td")
            .add(open.wg.klass("frame")))
          .add(Q("td")
            .add(close.wg.klass("frame"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 4).add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .klass("head")
            .text(_("Date")))
          .add(Q("td")
            .klass("head")
            .text(_("Open")))
          .add(Q("td")
            .klass("head")
            .text(_("CloseN"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 4)
            .add(Q("hr"))))
        .adds(list()))
    ;
  }

  function view () {
    generalWg();
    holidaysWg();
    specialDaysWg();
    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(Q("div")
          .klass("head")
          .text(_("Calendar")))
        .add(Q("table")
          .klass("main")
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .add(Wrule.mkBig(_("General Time Table")))))
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .add(generalDiv)))
          .add(Q("tr")
            .add(Q("td")
              .style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Holidays")))
              .add(holidaysDiv))
            .add(Q("td")
              .style("width:50%;vertical-align:top")
              .add(Wrule.mkBig(_("Special days")))
              .add(specialDaysDiv)))))
    ;
  }

  // Control -------------------------------------------------------------------

  function generalChange () {
    final tt = new Timetable(
      gopen.hour, gopen.minute,
      gclose.hour, gclose.minute
    );
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("calendar"),
      "rq" => Js.ws("setGeneral"),
      "timetable" => tt.toJs()
    ], rp -> {
    });
  }

  function holidaysChange (ls: Array<String>) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("calendar"),
      "rq" => Js.ws("setHolidays"),
      "holidays" => Js.wa(ls.map(e -> Js.ws(e)))
    ], rp -> {
      calendar = new Calendar(calendar.general, ls, calendar.specialDays);
      holidaysWg();
    });
  }

  function specialDaysChange (ls: Array<MarketDay>) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("calendar"),
      "rq" => Js.ws("setSpecialDays"),
      "specialDays" => Js.wa(ls.map(e -> e.toJs()))
    ], rp -> {
      calendar = new Calendar(calendar.general, calendar.holidays, ls);
      specialDaysWg();
    });
  }

  function addHoliday (ls: Array<String>, d: String) {
    final now = Dt.to(Date.now());
    if (d < now) {
      Msg.error(_("Date before today"), () -> {});
      return;
    }
    if (It.from(ls).contains(d)) {
      Msg.error(_args(_("Date '%0' is duplicated"), [d]), () -> {});
      return;
    }
    ls.push(d);
    holidaysChange(ls);
  }

  function delHoliday (ls: Array<String>, d: String) {
    if (Ui.confirm(_args(_("Delete '%0'?"), [d]))) {
      holidaysChange(ls.filter(e -> e != d));
    }
  }

  function addSpecialDay (
    ls: Array<MarketDay>, d: String, hopen, mopen, hclose, mclose: Int
  ) {
    final now = Dt.to(Date.now());
    if (d < now) {
      Msg.error(_("Date before today"), () -> {});
      return;
    }
    if (It.from(ls).indexf(e -> e.date == d) != -1) {
      Msg.error(_args(_("Date '%0' is duplicated"), [d]), () -> {});
      return;
    }
    ls.push(new MarketDay(d, hopen, mopen, hclose, mclose));
    specialDaysChange(ls);
  }

  function delSpecialDay (ls: Array<MarketDay>, d: String) {
    if (Ui.confirm(_args(_("Delete '%0'?"), [d]))) {
      specialDaysChange(ls.filter(e -> e.date != d));
    }
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("calendar"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final calendar = Calendar.fromJs(rp["calendar"]);
      new CalendarPg(wg, calendar);
    });
  }
}
