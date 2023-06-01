import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as ann from  "../data/ann.js";
import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as datePicker from  "../libdm/datePicker.js";
import * as daySelector from  "../wgs/daySelector.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Periodic",
    rq: "idata"
  }));
  const Anns =sys.$checkNull( arr.map(Rp.anns, ann.fromJs));
  arr.sort(Anns, function(A1, A2)  {sys.$params(arguments.length, 2);  return str.toUpper(A1.text) < str.toUpper(A2.text);});
  const Id =sys.$checkNull( [ -1]);

  const Show =sys.$checkNull( [[]]);

  
  
   async  function update(id, DayDs, hWg, mWg, txWg)  {sys.$params(arguments.length, 5);
    const Days =sys.$checkNull( DayDs.days);
    const tx =sys.$checkNull( txWg.getValue().trim());

    const Err =sys.$checkNull( [""]);
    if (sys.asBool(!sys.asBool(Days))) Err[0] =sys.$checkExists(Err[0],sys.$checkNull( II("Week days is missing")));
    if (sys.asBool(!sys.asBool(tx))) Err[0] =sys.$checkExists(Err[0],sys.$checkNull( II("Command value is missing")));

    if (sys.asBool(Err[0])) {
      ui.alert(Err[0]);
      return;
    }

    const today =sys.$checkNull( time.now());
    const tm =sys.$checkNull(  time.mk(
      time.day(today), time.month(today), time.year(today),
      math.fromStr(hWg.getValue())[0], math.fromStr(mWg.getValue())[0], 0
    ));
    const time0 =sys.$checkNull( math.toInt(tm / 1000));
    const time1 =sys.$checkNull( arr.map(Days, function(e)  {sys.$params(arguments.length, 1);  return math.toInt(e);}));

    await client.ssend({
      prg: cts.appName,
      source: "Periodic",
      rq:sys.asBool( sys.$eq(Id[0] ,  -1)) ? "new" : "modify",
      ann: ann.toJs(ann.mk(Id[0], ann.typePERIODIC, js.w([time0, time1]), tx))
    });
    mk(wg);
  };

  
   function edit(id)  {sys.$params(arguments.length, 1);
    Id[0] =sys.$checkExists(Id[0],sys.$checkNull( id));
    Show[0]();
  };

  
   function editCancel()  {sys.$params(arguments.length, 0);
    Id[0] =sys.$checkExists(Id[0],sys.$checkNull(  -1));
    Show[0]();
  };

  
   async  function del(id)  {sys.$params(arguments.length, 1);
    if (sys.asBool(!sys.asBool(ui.confirm(II("Delete annotation?"))))) return;

    await client.ssend({
      prg: cts.appName,
      source: "Periodic",
      rq: "delete",
      id:id
    });
    mk(wg);
  };

  
   async  function run(runSpan, id)  {sys.$params(arguments.length, 2);
    runSpan
      .removeAll()
      .add(ui.img("wait.gif"))
    ;

    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Periodic",
      rq: "run",
      id:id
    }));

    if (sys.asBool(Rp.error)) {
      ui.alert(Rp.error);
      mk(wg);
    } else {
      runSpan
        .removeAll()
        .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); run(runSpan, id);})
          .add(ui.img("run")))
      ;
    }
  };

  

  
   function th()  {sys.$params(arguments.length, 0);  return Q("td").style("text-align:center");};

  
   function trsNew()  {sys.$params(arguments.length, 0);
    const dayWg =sys.$checkNull( Q("div"));
    const DayDs =sys.$checkNull( daySelector.mk(dayWg, [], sys.$eq(Id[0] ,  -1)));
    DayDs.show();
    const hWg =sys.$checkNull( ui.select(
      "h-new", arr.fromIter(iter.map(iter.$range(0,24), function(i)  {sys.$params(arguments.length, 1);  return fns.formatN00(i);}))
    ).disabled(sys.$neq(Id[0] ,  -1)));
    const mWg =sys.$checkNull( ui.select(
      "m-new", arr.fromIter(iter.map(iter.$range(0,60), function(i)  {sys.$params(arguments.length, 1);  return fns.formatN00(i);}))
    ).disabled(sys.$neq(Id[0] ,  -1)));
    const txWg =sys.$checkNull( Q("textarea")
      .att("spellcheck", false)
      .att("cols", 60)
      .att("rows", 3)
      .disabled(sys.$neq(Id[0] ,  -1)))
    ;

     return [
      Q("tr")
        .add(Q("td")
          .att("colspan", "2"))
        .add(th()
          .text(II("Day")))
        .add(th()
          .att("colspan", "2")
          .text(II("Hour - Minute")))
        .add(th()
          .text(II("Command")))
        .add(Q("td")),
      Q("tr").add(Q("td").klass("line").att("colspan", "8")),
      Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(sys.asBool(sys.$neq(Id[0] ,  -1))
            ? ui.lightImg("add")
                .setStyle("vertical-align", "middle")
            : ui.link(function(ev)  {sys.$params(arguments.length, 1); update( -1, DayDs, hWg, mWg, txWg);})
              .add(ui.img("add")
                .style("vertical-align:middle"))))
        .add(Q("td")
          .add(dayWg))
        .add(Q("td")
          .add(hWg))
        .add(Q("td")
          .add(mWg))
        .add(Q("td")
          .add(txWg))
        .add(Q("td")),
      Q("tr").add(Q("td").att("colspan", "8").add(Q("hr")))
    ];
  };

  
   function mkTr(Ann)  {sys.$params(arguments.length, 1);
    const isSel =sys.$checkNull( sys.$eq(Ann.id , Id[0]));
    const isNew =sys.$checkNull( sys.$eq(Id[0] ,  -1));

    const dayWg =sys.$checkNull( Q("div"));
    const DayDs =sys.$checkNull( daySelector.mk(dayWg, ann.days(Ann), isSel));
    DayDs.show();

    const hWg =sys.$checkNull( ui.select(
        "h-new",
        arr.fromIter(iter.map(iter.$range(0,24), function(i)  {sys.$params(arguments.length, 1); 
          return (sys.asBool(sys.$eq(time.hour(ann.date(Ann)) , i)) ? "+" : "") + fns.formatN00(i)
        ;}))
      ).disabled(!sys.asBool(isSel)))
    ;
    const mWg =sys.$checkNull( ui.select(
        "m-new",
        arr.fromIter(iter.map(iter.$range(0,60), function(i)  {sys.$params(arguments.length, 1); 
          return (sys.asBool(sys.$eq(time.minute(ann.date(Ann)) , i)) ? "+" : "") + fns.formatN00(i)
        ;}))
      ).disabled(!sys.asBool(isSel)))
    ;


    const txWg =sys.$checkNull( Q("textarea")
      .att("spellcheck", false)
      .att("cols", 60)
      .att("rows", 3)
      .disabled(!sys.asBool(isSel))
      .value(Ann.text))
    ;
    const runSpan =sys.$checkNull( Q("span"));

     return Q("tr")
      .add(Q("td")
        .add(sys.asBool(isNew)
          ? ui.link(function(ev)  {sys.$params(arguments.length, 1); edit(Ann.id);})
            .add(ui.img("edit"))
          :sys.asBool( isSel)
            ? ui.link(function(ev)  {sys.$params(arguments.length, 1); editCancel();})
              .add(ui.img("cancel"))
            : ui.lightImg("edit")))
      .add(Q("td")
        .add(sys.asBool(isNew)
          ? ui.link(function(ev)  {sys.$params(arguments.length, 1); del(Ann.id);})
            .add(ui.img("delete"))
          :sys.asBool( isSel)
            ? ui.link(function(ev)  {sys.$params(arguments.length, 1); update(Ann.id, DayDs, hWg, mWg, txWg);})
              .add(ui.img("enter"))
            : ui.lightImg("delete")))
      .add(Q("td")
        .add(dayWg))
      .add(Q("td")
        .add(hWg))
      .add(Q("td")
        .add(mWg))
      .add(Q("td")
        .add(txWg))
      .add(Q("td")
        .add(runSpan
          .removeAll()
          .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); run(runSpan, Ann.id);})
            .add(ui.img("run")))))
    ;
  };

  
   function trs()  {sys.$params(arguments.length, 0);
    if (sys.asBool(sys.$eq(arr.size(Anns) , 0))) {
       return [
        Q("tr")
          .add(th()
          .att("colspan", "7")
          .klass("frame")
          .text(II("Without entries")))
      ];
    }

     return arr.map(Anns, function(Ann)  {sys.$params(arguments.length, 1);  return mkTr(Ann);});
  };

  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const tb =sys.$checkNull( Q("table")
      .att("align", "center")
      .adds(trsNew())
      .adds(trs()))
    ;

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(II("Periodic days")))
      .add(Q("div")
        .klass("separator"))
      .add(tb)
    ;
  });

  Show[0]();
};
