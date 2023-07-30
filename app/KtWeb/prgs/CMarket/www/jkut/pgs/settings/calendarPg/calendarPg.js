import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as datePicker from  "../../../libdm/datePicker.js";
import * as msg from  "../../../wgs/msg.js";
import * as wrule from  "../../../wgs/wrule.js";
import * as whour from  "../../../pgs/settings/calendarPg/wgs/whour.js";
import * as cts from  "../../../data/cts.js";
import * as fns from  "../../../data/fns.js";
import * as calendar from  "../../../data/calendar/calendar.js";
import * as timetable from  "../../../data/calendar/timetable.js";
import * as marketDay from  "../../../data/calendar/marketDay.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);




export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "CalendarPg",
    rq: "idata"
  }));
  const Calendar =sys.$checkNull( calendar.fromJs(Rp.calendar));

  const generalDiv =sys.$checkNull( Q("div").style("padding-bottom:15px"));
  const holidaysDiv =sys.$checkNull( Q("div"));
  const specialDaysDiv =sys.$checkNull( Q("div"));

  const Gopen =sys.$checkNull( [[]]);
  const Gclose =sys.$checkNull( [[]]);

  

  
   function generalChange()  {sys.$params(arguments.length, 0);
    const TT =sys.$checkNull( timetable.mk(
      whour.hour(Gopen[0]), whour.minute(Gopen[0]),
      whour.hour(Gclose[0]), whour.minute(Gclose[0])
    ));
    client.send({
    prg: cts.appName,
    module: "Settings",
    source: "CalendarPg",
    rq: "setGeneral",
    timetable: timetable.toJs(TT)
    });
  };

  
   async  function holidaysChange(Ls)  {sys.$params(arguments.length, 1);
    await client.send({
    prg: cts.appName,
    module: "Settings",
    source: "CalendarPg",
    rq: "setHolidays",
    holidays: Ls
    });
    mk(wg);
  };

  
   async  function specialDaysChange(Ls)  {sys.$params(arguments.length, 1);
    await client.send({
    prg: cts.appName,
    module: "Settings",
    source: "CalendarPg",
    rq: "setSpecialDays",
    specialDays: arr.map(Ls, marketDay.toJs)
    });
    mk(wg);
  };

  
   function addHoliday(Ls, d)  {sys.$params(arguments.length, 2);
    const now =sys.$checkNull( time.toStr(time.now()));
    if (sys.asBool(d < now)) {
      msg.error(II("Date before today"), function()  {sys.$params(arguments.length, 0);});
      return;
    }
    if (sys.asBool(arr.any(Ls, function(e)  {sys.$params(arguments.length, 1);  return sys.$eq(e , d);}))) {
      msg.error(i18n.fmt(II("Date '%0' is duplicated"), [d]), function()  {sys.$params(arguments.length, 0);});
      return;
    }
    arr.push(Ls, d);
    holidaysChange(Ls);
  };

  
   function delHoliday(Ls, d)  {sys.$params(arguments.length, 2);
    if (sys.asBool(ui.confirm(i18n.fmt(II("Delete '%0'?"), [d]))))
      holidaysChange(arr.filter(Ls, function(e)  {sys.$params(arguments.length, 1);  return sys.$neq(e , d);}));
  };

  
   function addSpecialDay(Ls, d, hopen, mopen, hclose, mclose)  {sys.$params(arguments.length, 6);
    const now =sys.$checkNull( time.toStr(time.now()));
    if (sys.asBool(d < now)) {
      msg.error(II("Date before today"), function()  {sys.$params(arguments.length, 0);});
      return;
    }
    if (sys.asBool(arr.any(Ls, function(S)  {sys.$params(arguments.length, 1);  return sys.$eq(S.date , d);}))) {
      msg.error(i18n.fmt(II("Date '%0' is duplicated"), [d]), function()  {sys.$params(arguments.length, 0);});
      return;
    }
    arr.push(Ls, marketDay.mk(d, hopen, mopen, hclose, mclose));
    specialDaysChange(Ls);
  };

  
   function delSpecialDay(Ls, d)  {sys.$params(arguments.length, 2);
    if (sys.asBool(ui.confirm(i18n.fmt(II("Delete '%0'?"), [d])))) {
      specialDaysChange(arr.filter(Ls, function(S)  {sys.$params(arguments.length, 1);  return sys.$neq(S.date , d);}));
    }
  };

  

  Gopen[0] =sys.$checkExists(Gopen[0],sys.$checkNull( whour.mk(
    Calendar.General.hopen, Calendar.General.mopen, generalChange
  )));
  Gclose[0] =sys.$checkExists(Gclose[0],sys.$checkNull( whour.mk(
    Calendar.General.hclose, Calendar.General.mclose, generalChange
  )));

  
   function generalWg()  {sys.$params(arguments.length, 0); generalDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .klass("head")
            .text(II("Open")))
          .add(Q("td")
            .klass("head")
            .text(II("CloseN"))))
        .add(Q("tr")
          .add(Q("td").style("padding:0px")
            .add(Q("hr")))
          .add(Q("td").style("padding:0px")
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .add(whour.wg(Gopen[0]).klass("frame")))
          .add(Q("td")
            .add(whour.wg(Gclose[0]).klass("frame")))))
    ;};

  
   function holidaysWg()  {sys.$params(arguments.length, 0);
    const Ls =sys.$checkNull( arr.copy(Calendar.Holidays));
    arr.sort(Ls, function(e1, e2)  {sys.$params(arguments.length, 2);  return e1 < e2;});

    
     function list()  {sys.$params(arguments.length, 0); return sys.asBool( Ls)
      ? arr.map(Ls, function(d)  {sys.$params(arguments.length, 1);  return Q("tr")
          .add(Q("td")
            .add(ui.link(function(e)  {sys.$params(arguments.length, 1); delHoliday(Ls, d);})
              .add(ui.img("minus"))))
          .add(Q("td")
            .style("text-align:center")
            .text(sys.asBool(sys.$eq(i18n.getLang() , "es"))
                ? time.toIso(time.fromStr(d))
                : time.toEn(time.fromStr(d))))
        ;})
      : [ Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .klass("frame4")
              .add(Q("tr")
                .add(Q("td")
                  .html(II("Without dates"))))))
        ]
      ;};

    const dpInput =sys.$checkNull( Q("input")
      .att("type", "text")
      .style("text-align:center;width:166px"))
    ;
    const dp =sys.$checkNull( datePicker.mk(
      sys.$eq(i18n.getLang() , "es"),
      time.now(),
      function(d)  {sys.$params(arguments.length, 1); if (sys.asBool(!sys.asBool(d))) {
          const dt =sys.$checkNull( time.now());
          datePicker.setDate(dp, dt);
          dpInput.value(sys.asBool(sys.$eq(i18n.getLang() , "es"))
            ? time.toIso(time.fromStr(dt))
            : time.toEn(time.fromStr(dt))
          );
        }}
    ));

    holidaysDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .add(ui.link(
                function(e)  {sys.$params(arguments.length, 1); addHoliday(Ls, time.toStr(datePicker.getDate(dp)[0]));}
              ).add(ui.img("plus"))))
          .add(Q("td")
            .add(datePicker.mkText(dp, dpInput))))
        .add(Q("tr")
          .add(Q("td").att("colspan", 2)
            .add(Q("hr"))))
        .adds(list()))
    ;
  };

  
   function specialDaysWg()  {sys.$params(arguments.length, 0);
    const Ls =sys.$checkNull( arr.copy(Calendar.SpecialDays));
    arr.sort(Ls, function(S1, S2)  {sys.$params(arguments.length, 2);  return S1.date < S2.date;});

  
     function list()  {sys.$params(arguments.length, 0); return sys.asBool( Ls)
      ? arr.map(Ls, function(S)  {sys.$params(arguments.length, 1);  return Q("tr")
          .add(Q("td")
            .add(ui.link(function(e)  {sys.$params(arguments.length, 1); delSpecialDay(Ls, S.date);})
              .add(ui.img("minus"))))
          .add(Q("td")
            .style("text-align:center")
            .text(sys.asBool(sys.$eq(i18n.getLang() , "es"))
                ? time.toIso(time.fromStr(S.date))
                : time.toEn(time.fromStr(S.date))))
          .add(Q("td")
            .style("text-align:center")
            .text(fns.format00(S.hopen) + ":" + fns.format00(S.mopen)))
          .add(Q("td")
            .style("text-align:center")
            .text(fns.format00(S.hclose) + ":" + fns.format00(S.mclose)))
        ;})
      : [ Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .klass("frame4")
              .add(Q("tr")
                .add(Q("td")
                  .html(II("Without dates"))))))
          .add(Q("td"))
          .add(Q("td"))
        ]
      ;};

    const dpInput =sys.$checkNull( Q("input")
      .att("type", "text")
      .style("text-align:center;width:166px"))
    ;
    const dp =sys.$checkNull( datePicker.mk(
      sys.$eq(i18n.getLang() , "es"),
      time.now(),
      function(d)  {sys.$params(arguments.length, 1); if (sys.asBool(!sys.asBool(d))) {
          const dt =sys.$checkNull( time.now());
          datePicker.setDate(dp, dt);
          dpInput.value(sys.asBool(sys.$eq(i18n.getLang() , "es"))
            ? time.toIso(time.fromStr(dt))
            : time.toEn(time.fromStr(dt))
          );
        }}
    ));

    const open =sys.$checkNull( whour.mk(whour.hour(Gopen[0]), whour.minute(Gopen[0]), function()  {sys.$params(arguments.length, 0);}));
    const close =sys.$checkNull( whour.mk(whour.hour(Gclose[0]), whour.minute(Gclose[0]), function()  {sys.$params(arguments.length, 0);}));
    specialDaysDiv
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1);
              addSpecialDay(
                Ls, time.toStr(datePicker.getDate(dp)[0]),
                whour.hour(open), whour.minute(open),
                whour.hour(close), whour.minute(close)
              );
            }).add(ui.img("plus"))))
          .add(Q("td")
            .add(datePicker.mkText(dp, dpInput)))
          .add(Q("td")
            .add(whour.wg(open).klass("frame")))
          .add(Q("td")
            .add(whour.wg(close).klass("frame"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 4).add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .klass("head")
            .text(II("Date")))
          .add(Q("td")
            .klass("head")
            .text(II("Open")))
          .add(Q("td")
            .klass("head")
            .text(II("CloseN"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 4)
            .add(Q("hr"))))
        .adds(list()))
    ;
  };

  generalWg();
  holidaysWg();
  specialDaysWg();

  wg
    .removeAll()
    .add(Q("div")
      .style("text-align:center")
      .add(Q("div")
        .klass("head")
        .text(II("Calendar")))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(wrule.mkBig(II("General Time Table")))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(generalDiv)))
        .add(Q("tr")
          .add(Q("td")
            .style("width:50%;vertical-align:top")
            .add(wrule.mkBig(II("Holidays")))
            .add(holidaysDiv))
          .add(Q("td")
            .style("width:50%;vertical-align:top")
            .add(wrule.mkBig(II("Special days")))
            .add(specialDaysDiv)))))
  ;
};
