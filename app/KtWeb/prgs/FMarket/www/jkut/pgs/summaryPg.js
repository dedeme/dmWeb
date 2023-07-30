import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as cts from  "../data/cts.js";
import * as flea from  "../data/flea.js";
import * as fns from  "../fns.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "SummaryPg",
    rq: "idata"
  }));

  
  
  
  const Summary =sys.$checkNull( Rp.summary);
  const Dates =sys.$checkNull( dic.keys(Summary));
  arr.sort(Dates, function(d1, d2)  {sys.$params(arguments.length, 2);  return d1 > d2;});

  const size =sys.$checkNull( arr.size(Dates));
  const iV =sys.$checkNull( [0]);
  const Trs =sys.$checkNull( []); 
  while (sys.asBool(true)) {
    if (sys.asBool(sys.$eq(iV[0] , size))) break;
    const tr =sys.$checkNull( Q("tr"));
    for (let i = 0;i < 4; ++i) {
      if (sys.asBool(sys.$eq(iV[0] , size))) {
        for (let j = i;j < 4; ++j) tr.add(Q("td"));
        break;
      }
      const d =sys.$checkNull( Dates[iV[0]]);
      tr.add(Q("td").add(mkDay(d, Summary[d])));
      arr.push(Trs, tr);
      iV[0] +=sys.$checkExists(iV[0],sys.$checkNull( 1));
    }
  }

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .adds(Trs))
  ;

};






 function mkDay(date, summary)  {sys.$params(arguments.length, 2);
  const BRows =sys.$checkNull( []); 
  const WRows =sys.$checkNull( []); 
  const ARows =sys.$checkNull( []); 

  for (let mdId  of sys.$forObject( dic.keys(summary))) {
    const S =sys.$checkNull( summary[mdId]);
    const B =sys.$checkNull( S[0]);
    arr.unshift(B, mdId);
    arr.push(BRows, B);
    const W =sys.$checkNull( S[1]);
    arr.unshift(W, mdId);
    arr.push(WRows, W);
    const A =sys.$checkNull( S[2]);
    arr.unshift(A, mdId);
    arr.push(ARows, A);
  }

   return Q("table")
    .klass("border")
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 4)
        .klass("borderWhite")
        .style("text-align:center")
        .text(time.toIso(time.fromStr(date)))))
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 4)
        .klass("header")
        .text(II("Best"))))
    .adds(mkResults(BRows))
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 4)
        .klass("header")
        .text(II("Average"))))
    .adds(mkResults(ARows))
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 4)
        .klass("header")
        .text(II("Worst"))))
    .adds(mkResults(WRows))
  ;
};




 function mkResults(Rs)  {sys.$params(arguments.length, 1);
  arr.sort(Rs, function(R1, R2)  {sys.$params(arguments.length, 2);  return R1[3] > R2[3];});
  const Trs =sys.$checkNull( []); 
  for (let R  of sys.$forObject( Rs)) {
    arr.push(Trs, Q("tr")
      .add(Q("td")
        .klass("borderWhite")
        .text(R[0]))
      .add(fns.mkTdN(R[1], 2))
      .add(fns.mkTdN(R[2], 4))
      .add(fns.mkTdN(R[3], 0))
    );
  }
   return Trs;
};
