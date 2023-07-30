import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as cts from  "../data/cts.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(wg, isNear, ParNearStart, ParFarStart, ParStep, Results)  {sys.$params(arguments.length, 6);
  const Url =sys.$checkNull( ui.url());

  const incS =sys.$checkNull( ParStep[0]);
  const incA =sys.$checkNull( ParStep[1]);
  const ParStart =sys.$checkNull(sys.asBool( isNear) ? ParNearStart : ParFarStart);
  const Rows =sys.$checkNull( []); 
  const sV =sys.$checkNull( [ParStart[0]]); 
  for (let RR  of sys.$forObject( Results)) {
    const aV =sys.$checkNull( [ParStart[1]]); 
    for (let R  of sys.$forObject( RR)) {
      arr.push(
        Rows,
        [math.round(sV[0], 4), math.round(aV[0], 4), R[0], R[1], R[2], R[3]]
      );
      aV[0] +=sys.$checkExists(aV[0],sys.$checkNull( incA));
    }
    sV[0] +=sys.$checkExists(sV[0],sys.$checkNull( incS));
  }
  arr.sort(Rows, function(R1, R2)  {sys.$params(arguments.length, 2); return sys.asBool( sys.$eq(R1[4] , R2[4]))
    ? R1[2] > R2[2]
    : R1[4] > R2[4]
  ;});

  

  
   function center(pStart, pAppr)  {sys.$params(arguments.length, 2);
    const incNS =sys.$checkNull( math.round(sys.asBool(isNear) ? incS : incS / 2, 4));
    const incNA =sys.$checkNull( math.round(sys.asBool(isNear) ? incA : incA / 2, 4));
    const sTop =sys.$checkNull( ParFarStart[0] + cts.simSteps * incNS);
    const aTop =sys.$checkNull( ParFarStart[1] + cts.simSteps * incNA);

    const start0 =sys.$checkNull( pStart - (cts.simSteps / 2) * incNS);
    const start =sys.$checkNull( math.round(sys.asBool(
      start0 >= ParFarStart[0])
        ?sys.asBool( start0 <= sTop)
          ? start0
          : sTop
        : ParFarStart[0]
      , 4
    ));
    const appr0 =sys.$checkNull( pAppr - (cts.simSteps / 2) * incNA);
    const appr =sys.$checkNull( math.round(sys.asBool(
      appr0 >= ParFarStart[1])
        ?sys.asBool( appr0 <= aTop)
          ? appr0
          : aTop
        : ParFarStart[1],
      4
    ));

    const currentStart =sys.$checkNull( ParNearStart[0]);
    const currentAppr =sys.$checkNull( ParNearStart[1]);

    if (sys.asBool(!sys.asBool(ui.confirm(i18n.fmt(
      II("Change near start corner from\n[%0, %1]\nto\n[%2, %3]?"),
      [currentStart, currentAppr, start, appr]
    ))))) return;

    client.send({
      prg: "DeepMarket",
      source: "ListPg",
      rq: "center",
      days: "[" + Url["0"] + "-" + Url["1"] + "]",
      start:start,
      appr:appr
    });

    ui.alert(i18n.fmt(
      II("Soon the near star corner will be changed to\n[%0, %1]"),
      [start, appr]
    ));
  };

  

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td"))
        .add(Q("td")
          .klass("rhead")
          .text(II("Start")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Appr.")))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rhead")
          .text(II("Assets")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Prfs.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Eval.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Sls."))))
      .adds(arr.map(Rows, function(R)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); center(R[0], R[1]);})
            .klass("link")
            .add(ui.img("center"))))
        .add(Q("td")
          .add(Q("a")
            .klass("link")
            .att(
                "href",
                "?" + Url["0"] +
                "&" + Url["1"] +
                "&charts" +
                "&" + R[0] +
                "&" + R[1]
              )
            .add(ui.img("see"))))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[0], 4)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[1], 4)))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[2], 2)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[3] * 100, 2) + "%"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[4], 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(R[5], 0)));}
        )))
  ;
};
