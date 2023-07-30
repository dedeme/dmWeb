import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as cts from  "../data/cts.js";
import * as alarm from  "../data/alarm.js";
import * as main from  "../main.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Home",
    rq: "idata"
  }));

  const Alarms =sys.$checkNull( Rp.alarms);
  arr.sort(Alarms, function(a1, a2)  {sys.$params(arguments.length, 2);  return a1[1] < a2[1];});

  const butonSpan =sys.$checkNull( Q("span"));
  const entry =sys.$checkNull( ui.field("_accept")
    .style("width:50px"))
  ;
  const accept =sys.$checkNull( Q("button")
    .att("id", "_accept")
    .text(II("Add")))
  ;

  

   async  function add(t)  {sys.$params(arguments.length, 1);
    
     function badFormat()  {sys.$params(arguments.length, 0);
      const or =sys.$checkNull( II("or"));
       return II("Bad Format. Use") + ":\n"+
        "  HH,MM " + or + " HH.MM " + or + " HH:MM"
      ;
    };

    const sep =sys.$checkNull(sys.asBool( t.includes(","))
      ? ","
      :sys.asBool( t.includes("."))
        ? "."
        :sys.asBool( t.includes(":"))
          ? ":"
          : "");
    if (sys.asBool(sys.$eq(sep , ""))) {
      ui.alert(II("Separator is missing"));
      return;
    }

    const parts =sys.$checkNull( t.split(sep));
    if (sys.asBool(sys.$neq(parts.length , 2))) {
      ui.alert(badFormat());
      return;
    }
    const H =sys.$checkNull( math.fromStr(parts[0]));
    const ms =sys.$checkNull(sys.asBool( sys.$eq(parts[1].length , 1)) ? "0" + parts[1] : parts[1]);
    const M =sys.$checkNull( math.fromStr(ms));
    if (sys.asBool(sys.asBool(!sys.asBool(H)) || sys.asBool(!sys.asBool(M)))) {
      ui.alert(badFormat());
      return;
    }
    const h =sys.$checkNull( H[0]);
    const m =sys.$checkNull( M[0]);
    if (sys.asBool(sys.asBool(h < 0) || sys.asBool(h > 23))) {
      ui.alert(II("Hour out of range"));
      return;
    }
    if (sys.asBool(sys.asBool(m < 0) || sys.asBool(m > 59))) {
      ui.alert(II("Minutes out of range"));
      return;
    }

    const tm =sys.$checkNull( time.now());
    const hnow =sys.$checkNull( time.hour(tm));
    const mnow =sys.$checkNull( time.minute(tm));

    const dayAlarm =sys.$checkNull(sys.asBool( sys.asBool(h > hnow) || sys.asBool((sys.asBool(sys.$eq(hnow , h)) && sys.asBool(m > mnow))))
      ? tm
      : time.addDays(tm, 1))
    ;
    butonSpan.removeAll().add(ui.img("wait.gif"));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Home",
      rq: "add",
      key: cryp.genK(6) + ":" + tm,
      tm: time.mk(
          time.day(dayAlarm), time.month(dayAlarm), time.year(dayAlarm),
          h, m, 0
        )
    }));

    if (sys.asBool(Rp.isDup)) ui.alert(II("Duplicated alarm"));
    window.location.reload(true);
  };

  
   async  function del(A)  {sys.$params(arguments.length, 1);
    if (sys.asBool(ui.confirm(II("Remove the alarm") + " '" + alarm.timeToStr(A) + "'"))) {
      await client.ssend({
        prg: cts.appName,
        source: "Home",
        rq: "del",
        alarm: A
      });
      mk(wg);
    }
  };

  

  accept.on("click", function(e)  {sys.$params(arguments.length, 1); add(entry.getValue());});

  const trs =sys.$checkNull( arr.map(Alarms, function(A)  {sys.$params(arguments.length, 1);  return Q("tr")
      .add(Q("td")
        .klass("frame")
        .style("text-align: right")
        .text(alarm.timeToStr(A)))
      .add(Q("td")
        .style("Text-align: left")
        .add(ui.link(function(e)  {sys.$params(arguments.length, 1); del(A);})
          .add(ui.img("delete"))))
    ;}));

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("div")
            .klass("head")
            .text(II("New Alarm")))))
      .add(Q("tr")
        .add(Q("td")
          .add(entry))
        .add(Q("td")
          .add(butonSpan
            .add(accept))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("div")
            .klass("head")
            .text(II("Programmed Alarms")))))
      .adds(sys.asBool(sys.$eq(Alarms.length , 0))
          ? [Q("tr")
              .add(Q("td")
                .klass("frame")
                .att("colspan", 2)
                .style("text-align:center")
                .text(II("Without Alarms")))
            ]
          : trs
        ))
  ;
  entry.e.focus();
};
