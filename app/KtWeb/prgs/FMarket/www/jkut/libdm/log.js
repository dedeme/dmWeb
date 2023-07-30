import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';






































































const Q =sys.$checkNull( ui.q);












export  async  function mk(wg, load, reset, tlt, minified, lineWidth, linesNumber)  {sys.$params(arguments.length, 7);
  const Rows =sys.$checkNull( []);
  await load(function(Rs)  {sys.$params(arguments.length, 1); arr.each(Rs, function(LgRow)  {sys.$params(arguments.length, 1); arr.push(Rows, LgRow);});});

  const Minified =sys.$checkNull( [minified]);
  const Is2Days =sys.$checkNull( [false]);
  const IsErrors =sys.$checkNull( [true]);

  const tableFrame =sys.$checkNull( "background-color: rgb(240, 245, 250);" +
    "border: 1px solid rgb(110,130,150);" +
    "font-family: sans;font-size: 14px;" +
    "padding: 4px;border-radius: 4px;")
  ;

  const Show =sys.$checkNull( [[]]);
  const View1 =sys.$checkNull( [[]]);
  const View2 =sys.$checkNull( [[]]);

  

  
   function on2Days()  {sys.$params(arguments.length, 0);
    Is2Days[0] =sys.$checkExists(Is2Days[0],sys.$checkNull( true));
    View1[0]();
  };

  
   function onAllD()  {sys.$params(arguments.length, 0);
    Is2Days[0] =sys.$checkExists(Is2Days[0],sys.$checkNull( false));
    View1[0]();
  };

  
   function onReload()  {sys.$params(arguments.length, 0); load(function(Rs)  {sys.$params(arguments.length, 1);
      arr.clear(Rows);
      for (let E  of sys.$forObject( Rs)) arr.push(Rows, E);
      Show[0]();
    });};

  
   function onDelete()  {sys.$params(arguments.length, 0);
    if (sys.asBool(ui.confirm(tlt("All log entries will be deleted.\nContinue?"))))
      reset(function()  {sys.$params(arguments.length, 0); onReload();});};

  
   function onErrors()  {sys.$params(arguments.length, 0);
    IsErrors[0] =sys.$checkExists(IsErrors[0],sys.$checkNull( true));
    View1[0]();
  };

  
   function onAll()  {sys.$params(arguments.length, 0);
    IsErrors[0] =sys.$checkExists(IsErrors[0],sys.$checkNull( false));
    View1[0]();
  };

  

  
   function led()  {sys.$params(arguments.length, 0);
    const warns =sys.$checkNull( sys.$neq(arr.size(Rows) , 0));
    const errs =sys.$checkNull( arr.any(Rows, function(E)  {sys.$params(arguments.length, 1);  return E.isError;}));
     return Q("td")
      .style("text-align:center;width:20%")
      .add(Q("span")
        .style(
            "border: 1px solid rgb(110,130,150);" +
            "border-radius: 8px;" +
            "background: " +
              (sys.asBool(errs) ? "#e04040" :sys.asBool( warns) ? "#e0e040" : "#ffffff") + ";" +
            "cursor:pointer"
          )
        .html("&nbsp;&nbsp;")
        .on("click", function(ev)  {sys.$params(arguments.length, 1);
            Minified[0] =sys.$checkExists(Minified[0],sys.$checkNull( !sys.asBool(Minified[0])));
            Show[0]();
          }))
    ;
  };

  
   function view1()  {sys.$params(arguments.length, 0);
    
     function mkOption(isSel, id, action)  {sys.$params(arguments.length, 3);
      const frame =sys.$checkNull( "background-color: rgb(250, 250, 250);" +
        "border: 1px solid rgb(110,130,150);" +
        "padding: 4px;border-radius: 4px;")
      ;
      const link =sys.$checkNull( "text-decoration: none;color: #000080;" +
        "font-weight: normal;cursor:pointer;")
      ;
      const r =sys.$checkNull(sys.asBool( isSel)
        ? Q("span").style(frame)
        : ui.link(function(ev)  {sys.$params(arguments.length, 1); action();}).style(link))
      ;
       return r.text(id);
    };

    const lmenu =sys.$checkNull( Q("div"));
    const rmenu =sys.$checkNull( Q("div"));
    const area =sys.$checkNull( Q("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", linesNumber).att("cols", lineWidth + 5));

    lmenu
      .add(Q("span")
        .add(mkOption(Is2Days[0], tlt("2 Days"), function()  {sys.$params(arguments.length, 0); on2Days();})))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(!sys.asBool(Is2Days[0]), tlt("All"), function()  {sys.$params(arguments.length, 0); onAllD();})))
    ;

    rmenu
      .add(Q("span")
        .add(mkOption(false, tlt("Reload"), function()  {sys.$params(arguments.length, 0); onReload();})))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(false, tlt("Delete"), function()  {sys.$params(arguments.length, 0); onDelete();})))
      .add(Q("span").html(" | "))
      .add(Q("span")
        .add(mkOption(IsErrors[0], tlt("Errors"), function()  {sys.$params(arguments.length, 0); onErrors();})))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(!sys.asBool(IsErrors[0]), tlt("All"), function()  {sys.$params(arguments.length, 0); onAll();})))
    ;
    const today =sys.$checkNull( time.now());
    const Log =sys.$checkNull( arr.reverse(Rows));
    area.value(
      arr.join(
        arr.map(
          arr.filter(
            Log,
            function(E)  {sys.$params(arguments.length, 1); 
              return sys.asBool((sys.asBool(Is2Days[0]) ? time.dfDays(today, logRowDate(E)) < 3 : true)) &&
              sys.asBool((sys.asBool(IsErrors[0]) ? E.isError : true))
            ;}
          ),
          function(E)  {sys.$params(arguments.length, 1);  return logRowFormat(E, lineWidth);}
        ), "\n"
      )
    );

    wg
      .removeAll()
      .add(Q("div").klass("head").style("padding-bottom:10px").text(tlt("Log")))
      .add(Q("table").att("align", "center").style(tableFrame)
        .add(Q("tr")
          .add(Q("td").style("text-align:left;width:40%")
            .add(lmenu))
          .add(led())
          .add(Q("td").style("text-align:right;widht:80%")
            .add(rmenu)))
        .add(Q("tr").add(Q("td").att("colspan", 3)))
        .add(Q("tr")
          .add(Q("td").att("colspan", 3).add(area))))
    ;
  };
  View1[0] =sys.$checkExists(View1[0],sys.$checkNull( view1));

  
   function view2()  {sys.$params(arguments.length, 0); wg
      .removeAll()
      .add(Q("div").klass("head").style("padding-bottom:10px").text(tlt("Log")))
      .add(Q("table").att("align", "center").style(tableFrame)
        .add(Q("tr")
          .add(Q("tr")
            .add(led()))))
  ;};
  View2[0] =sys.$checkExists(View2[0],sys.$checkNull( view2));

  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0); if (sys.asBool(Minified[0])) view2(); else view1();});

  Show[0]();
};









export  function mkLogRow(isError, tm, msg)  {sys.$params(arguments.length, 3);  return {isError:isError, tm:tm, msg:msg};};



 function logRowDate(LgR)  {sys.$params(arguments.length, 1); return sys.asBool( sys.$eq(LgR.tm[2] , "-"))
    ? time.fromEn(str.trim(sys.$slice(LgR.tm,null,str.index(LgR.tm, "("))), "-")
    : time.fromIso(str.trim(sys.$slice(LgR.tm,null,str.index(LgR.tm, "("))), "/")
  ;};


 function format2(msg, indent, len)  {sys.$params(arguments.length, 3);
  if (sys.asBool(sys.$eq(str.trim(msg) , "")))  return msg;

  const R =sys.$checkNull( []);
  for (let l  of sys.$forObject( str.split(msg, "\n"))) {
    const Subr =sys.$checkNull( []);

    const L =sys.$checkNull( [l]);
    while (sys.asBool(str.len(L[0]) > len)) {
      const Line =sys.$checkNull( [sys.$slice(L[0],null,len)]);
      L[0] =sys.$checkExists(L[0],sys.$checkNull( sys.$slice(L[0],len,null)));
      const ix =sys.$checkNull( str.lastIndex(Line[0], " "));
      if (sys.asBool(sys.asBool(sys.$neq(ix ,  -1)) && sys.asBool(sys.$neq(str.trim(sys.$slice(Line[0],null,ix)) , "")))) {
        L[0] =sys.$checkExists(L[0],sys.$checkNull( sys.$slice(Line[0],ix + 1,null) + L[0]));
        Line[0] =sys.$checkExists(Line[0],sys.$checkNull( sys.$slice(Line[0],null,ix)));
      }
      arr.push(Subr, Line[0]);
    }

    if (sys.asBool(sys.$neq(str.trim(L[0]) , ""))) arr.push(Subr, L[0]);
    for (let subl  of sys.$forObject( Subr)) arr.push(R, subl);
  }

  const Ind =sys.$checkNull( [""]);
  for (let i = 0;i < indent; ++i) Ind[0] +=sys.$checkExists(Ind[0],sys.$checkNull( " "));
   return arr.join(R, "\n" + Ind[0]);
};




 function logRowFormat(LgR, lineWidth)  {sys.$params(arguments.length, 2);
  const indent =sys.$checkNull( str.len(LgR.tm) + 3);
  const len =sys.$checkNull( lineWidth - indent);
  const sep =sys.$checkNull(sys.asBool( LgR.isError) ? " = " : " - ");
   return LgR.tm + sep + format2(LgR.msg, indent, len);
};


export  function logRowFromJs(A)  {sys.$params(arguments.length, 1);  return mkLogRow(A[0], A[1], A[2]);};
