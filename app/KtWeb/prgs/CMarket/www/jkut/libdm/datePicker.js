import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);


const isEs =sys.$checkNull( 0);

const date =sys.$checkNull( isEs + 1);

const fn =sys.$checkNull( date + 1);

const dateView =sys.$checkNull( fn + 1);

const floating =sys.$checkNull( dateView + 1);

const elMonth =sys.$checkNull( floating + 1);

const elYear =sys.$checkNull( elMonth + 1);

const elDays =sys.$checkNull( elYear + 1);

const exTr =sys.$checkNull( elDays + 1);

const tr4 =sys.$checkNull( exTr + 1);

const tb =sys.$checkNull( tr4 + 1);








export  function mk(isEs, date, fn)  {sys.$params(arguments.length, 3);  return [
    isEs, [date], fn,
    time.mkDate(1, time.month(date), time.year(date)), 
    false, 
    Q("span"), 
    Q("span"), 
    [], 
    Q("tr"), 
    Q("tr"), 
    Q("table") 
  ];};



export  function getDate(Dp)  {sys.$params(arguments.length, 1);  return Dp[date];};




export  function setDate(Dp, newDate)  {sys.$params(arguments.length, 2);
  Dp[date] =sys.$checkExists(Dp[date],sys.$checkNull( [newDate]));
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(1, time.month(newDate), time.year(newDate))));
};


 function months(Dp)  {sys.$params(arguments.length, 1); return sys.asBool( Dp[isEs])
    ? ["ene", "feb", "mar", "abr", "may", "jun", "jul",
      "ago", "sep", "oct", "nov", "dic"]
    : ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
      "Aug", "Sep", "Oct", "Nov", "Dec"]
  ;};


 function weekDays(Dp)  {sys.$params(arguments.length, 1); return sys.asBool( Dp[isEs])
    ? ["dom", "lun", "mar", "mié", "jue", "vie", "sáb"]
    : ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  ;};


 function i18n(Dp)  {sys.$params(arguments.length, 1); return sys.asBool( Dp[isEs])
    ? {firstWeekDay: 1, today: "Hoy", none: "Nada"}
    : {firstWeekDay: 0, today: "Today", none: "None"}
  ;};


 function load(Dp)  {sys.$params(arguments.length, 1);
  Dp[elMonth].html(months(Dp)[time.month(Dp[dateView]) - 1]);
  Dp[elYear].html("" + time.year(Dp[dateView]));

  const ix0 =sys.$checkNull( time.weekday(Dp[dateView]) - i18n(Dp).firstWeekDay);
  const ix =sys.$checkNull(sys.asBool( ix0 < 0) ? 7 + ix0 : ix0);
  const month =sys.$checkNull( time.month(Dp[dateView]));
  const Date1 =sys.$checkNull( [time.mkDate(time.day(Dp[dateView]) - ix, month, time.year(Dp[dateView]))]);

  const today =sys.$checkNull( time.now());
  const tyear =sys.$checkNull( time.year(today));
  const tmonth =sys.$checkNull( time.month(today));
  const tday =sys.$checkNull( time.day(today));

  const Dyear =sys.$checkNull( [tyear]);
  const Dmonth =sys.$checkNull( [tmonth]);
  const Dday =sys.$checkNull( [tday]);

  if (sys.asBool(Dp[date])) {
    Dyear[0] =sys.$checkExists(Dyear[0],sys.$checkNull( time.year(Dp[date][0])));
    Dmonth[0] =sys.$checkExists(Dmonth[0],sys.$checkNull( time.month(Dp[date][0])));
    Dday[0] =sys.$checkExists(Dday[0],sys.$checkNull( time.day(Dp[date][0])));
  }

  const ExtraRow =sys.$checkNull( [false]);
  iter.each(iter.$range(0,6), function(i)  {sys.$params(arguments.length, 1);
    if (sys.asBool(sys.asBool(sys.$eq(i , 5)) && sys.asBool(sys.$eq(time.month(Date1[0]) , month)))) ExtraRow[0] =sys.$checkExists(ExtraRow[0],sys.$checkNull( true));
    iter.each(iter.$range(0,7), function(j)  {sys.$params(arguments.length, 1);
      const d =sys.$checkNull( Dp[elDays][i][j].removeAll());
      const year1 =sys.$checkNull( time.year(Date1[0]));
      const month1 =sys.$checkNull( time.month(Date1[0]));
      const day1 =sys.$checkNull( time.day(Date1[0]));

      if (sys.asBool(sys.asBool(sys.asBool(sys.$eq(day1 , Dday[0])) && sys.asBool(sys.$eq(month1 , Dmonth[0]))) && sys.asBool(sys.$eq(year1 , Dyear[0])))) {
        d.klass("select");
      } else {
        d.klass("day");
        if (sys.asBool(sys.$neq(time.month(Date1[0]) , month))) d.klass("dayOut");
        if (sys.asBool(sys.asBool(sys.$eq(time.weekday(Date1[0]) , 6)) || sys.asBool(sys.$eq(time.weekday(Date1[0]) , 0)))) {
          d.klass("weekend");
          if (sys.asBool(sys.$neq(time.month(Date1[0]) , month))) d.klass("weekendOut");
        }
      }
      if (sys.asBool(sys.asBool(sys.asBool(sys.$eq(day1 , tday)) && sys.asBool(sys.$eq(month1 , tmonth))) && sys.asBool(sys.$eq(year1 , tyear))))
        d.klass("today");

      const ddate1 =sys.$checkNull( Date1[0]);
      d.html("<span class='day'>" + time.day(ddate1) + "</span>");
      d.att("id", math.toStr(ddate1));
      Date1[0] =sys.$checkExists(Date1[0],sys.$checkNull( time.mkDate(
        time.day(Date1[0]) + 1, time.month(Date1[0]), time.year(Date1[0])
      )));
    });
  });

  if (sys.asBool(sys.$eq(Dp[tb].getAtt("hasTrEx") , "true"))) {
    Dp[tb].remove(Dp[exTr]);
    Dp[tb].att("hasTrEx", "false");
  }

  if (sys.asBool(ExtraRow[0])) {
    Dp[tb].remove(Dp[tr4]);

    Dp[tb].add(Dp[exTr]).add(Dp[tr4]);
    Dp[tb].att("hasTrEx", "true");
  }
};


 function previousMonth(Dp)  {sys.$params(arguments.length, 1);
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(
    1, time.month(Dp[dateView]) - 1, time.year(Dp[dateView])
  )));
  load(Dp);
};


 function nextMonth(Dp)  {sys.$params(arguments.length, 1);
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(
    1, time.month(Dp[dateView]) + 1, time.year(Dp[dateView])
  )));
  load(Dp);
};


 function previousYear(Dp)  {sys.$params(arguments.length, 1);
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(
    1, time.month(Dp[dateView]), time.year(Dp[dateView]) - 1
  )));
  load(Dp);
};


 function nextYear(Dp)  {sys.$params(arguments.length, 1);
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(
    1, time.month(Dp[dateView]), time.year(Dp[dateView]) + 1
  )));
  load(Dp);
};


 function today(Dp)  {sys.$params(arguments.length, 1);
  const today =sys.$checkNull( time.now());
  Dp[date][0] =sys.$checkExists(Dp[date][0],sys.$checkNull( today));
  Dp[dateView] =sys.$checkExists(Dp[dateView],sys.$checkNull( time.mkDate(1, time.month(today), time.year(today))));
  load(Dp);
};



 function none(Dp)  {sys.$params(arguments.length, 1);
  Dp[date] =sys.$checkExists(Dp[date],sys.$checkNull( []));
  load(Dp);
  Dp[fn]("");
};



 function clickDay(Dp, newDate)  {sys.$params(arguments.length, 2);
  Dp[date] =sys.$checkExists(Dp[date],sys.$checkNull( [newDate]));
  load(Dp);
  Dp[fn](time.toStr(newDate));
};




export  function mkWg(Dp)  {sys.$params(arguments.length, 1);
  
   function mkArrow(tx, f)  {sys.$params(arguments.length, 2);  return Q("td")
    .klass("arrow")
    .add(Q("span")
      .html(tx)
      .on("click", function(e)  {sys.$params(arguments.length, 1); f();}))
  ;};

  
   function mkHeader(colspan, txarr1, farr1, element, txarr2, farr2)  {sys.$params(arguments.length, 6);  return Q("td")
    .att("colspan", colspan)
    .add(Q("table")
      .klass("in")
      .add(Q("tr")
        .add(mkArrow(txarr1, farr1))
        .add(Q("td")
          .style("vertical-align:bottom")
          .add(element.klass("title")))
        .add(mkArrow(txarr2, farr2))))
  ;};

  Dp[elMonth] =sys.$checkExists(Dp[elMonth],sys.$checkNull( Q("span")));
  Dp[elYear] =sys.$checkExists(Dp[elYear],sys.$checkNull( Q("span")));
  Dp[elDays] =sys.$checkExists(Dp[elDays],sys.$checkNull( []));

  Dp[tr4] =sys.$checkExists(Dp[tr4],sys.$checkNull( Q("tr")
    .add(Q("td")
      .att("colspan", 4)
      .klass("left")
      .add(Q("span").klass("link")
        .html(i18n(Dp).today)
        .on("click", function(e)  {sys.$params(arguments.length, 1); today(Dp);})))
    .add(Q("td")
      .att("colspan", 3)
      .klass("right")
      .add(Q("span")
        .klass("link")
        .html(i18n(Dp).none)
        .on("click", function(e)  {sys.$params(arguments.length, 1); none(Dp);})))));

  Dp[tb] =sys.$checkExists(Dp[tb],sys.$checkNull( Q("table")
    .att("hasTrEx", "false")
    .klass("dmDatePicker")
    .add(Q("tr")
      .add(mkHeader(
        3, "&laquo",
        function()  {sys.$params(arguments.length, 0); previousMonth(Dp);},
        Dp[elMonth],
        "&raquo;",
        function()  {sys.$params(arguments.length, 0); nextMonth(Dp);}
      ))
      .add(Q("td"))
      .add(mkHeader(
        3, "&laquo",
        function()  {sys.$params(arguments.length, 0); previousYear(Dp);},
        Dp[elYear],
        "&raquo;",
        function()  {sys.$params(arguments.length, 0); nextYear(Dp);}
      )))
    .add(Q("tr")
      .adds(arr.fromIter(iter.map(iter.$range(0,7), function(i)  {sys.$params(arguments.length, 1);
        const ix0 =sys.$checkNull( i + i18n(Dp).firstWeekDay);
        const ix =sys.$checkNull(sys.asBool( ix0 > 6) ? ix0 - 7 : ix0);
         return Q("td")
          .html(weekDays(Dp)[ix])
        ;
      }))))
    .adds((function()  {sys.$params(arguments.length, 0);
        const Rows =sys.$checkNull( arr.fromIter(iter.map(iter.$range(0,5), function(i)  {sys.$params(arguments.length, 1);
          const Tds =sys.$checkNull( []);
          const tr =sys.$checkNull( Q("tr")
            .adds(arr.fromIter(iter.map(iter.$range(0,7), function(j)  {sys.$params(arguments.length, 1);
              const td =sys.$checkNull( Q("td"));
              td.on("click", function(e)  {sys.$params(arguments.length, 1); clickDay(Dp, math.fromStr(td.getAtt("id"))[0]);});
              arr.push(Tds, td);
               return td;
            }))))
          ;
          Dp[elDays].push(Tds);
           return tr;
        })));
        const Tds =sys.$checkNull( []);
        Dp[exTr] =sys.$checkExists(Dp[exTr],sys.$checkNull( Q("tr")
          .adds(arr.fromIter(iter.map(iter.$range(0,7), function(i)  {sys.$params(arguments.length, 1);
            const td =sys.$checkNull( Q("td"));
            td.on("click", function(e)  {sys.$params(arguments.length, 1); clickDay(Dp, math.fromStr(td.getAtt("id"))[0]);});
            arr.push(Tds, td);
             return td;
          })))))
        ;
        Dp[elDays].push(Tds);
         return Rows;
      })())
    .add(Dp[tr4])));
  load(Dp);
   return Q("div")
    .style(sys.asBool(Dp[floating]) ? "position:absolute" : "position:relative")
    .add(Dp[tb])
  ;
};





export  function mkButton(Dp, button)  {sys.$params(arguments.length, 2);
  const span =sys.$checkNull( Q("span"));
  const IsShow =sys.$checkNull( [false]);

  
   function btAction(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(!sys.asBool(IsShow[0]))) {
      span.add(mkWg(Dp));
      IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( true));
      return;
    }
    span.removeAll();
    IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( false));
  };
  button.on("click", btAction);

  const previousFn =sys.$checkNull( Dp[fn]);
  Dp[fn] =sys.$checkExists(Dp[fn], function(s)  {sys.$params(arguments.length, 1);
    previousFn(s);
    span.removeAll();
    IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( false));
  });

  Dp[floating] =sys.$checkExists(Dp[floating],sys.$checkNull( true));
   return Q("span")
    .add(button)
    .add(span)
  ;
};





export  function mkText(Dp, textInput)  {sys.$params(arguments.length, 2);
  
   function format(s)  {sys.$params(arguments.length, 1);
    const d =sys.$checkNull( time.fromStr(s));
    return sys.asBool( Dp[isEs]) ? time.toIso(d) : time.toEn(d);
  };
  const span =sys.$checkNull( Q("span"));
  const IsShow =sys.$checkNull( [false]);

  
   function btAction(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(!sys.asBool(IsShow[0]))) {
      span.add(mkWg(Dp));
      IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( true));
      return;
    }
    span.removeAll();
    IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( false));
  };

  const Date =sys.$checkNull( getDate(Dp));
  const val =sys.$checkNull(sys.asBool( Date) ? format(time.toStr(Date[0])) : "");
  textInput.value(val);
  textInput.on("click", btAction);
  textInput.on("keydown", function(e)  {sys.$params(arguments.length, 1);  e.preventDefault();;});

  const previousFn =sys.$checkNull( Dp[fn]);
  Dp[fn] =sys.$checkExists(Dp[fn], function(s)  {sys.$params(arguments.length, 1);
    textInput.value(sys.asBool(sys.$eq(s , "")) ? "" : format(s));
    previousFn(s);
    span.removeAll();
    IsShow[0] =sys.$checkExists(IsShow[0],sys.$checkNull( false));
  });

  Dp[floating] =sys.$checkExists(Dp[floating],sys.$checkNull( true));
   return Q("span")
    .add(textInput)
    .add(span)
  ;
};
