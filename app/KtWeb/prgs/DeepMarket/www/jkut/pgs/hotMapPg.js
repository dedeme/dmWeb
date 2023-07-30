import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as cts from  "../data/cts.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function color(max, min, value)  {sys.$params(arguments.length, 3);
  const df =sys.$checkNull( max - min);
  const red =sys.$checkNull( math.toInt((max - value) * 256 / df ));
  const blue =sys.$checkNull( math.toInt((value - min) * 256 / df ));
   return "rgb(" + red + ",80," + blue + ")";
};



export  function mk(wg, ParStart, ParStep, Results)  {sys.$params(arguments.length, 4);
  const Url =sys.$checkNull( ui.url());
  const MaxMin =sys.$checkNull( [Results[0][0][2], Results[0][0][2]]);
  for (let Row  of sys.$forObject( Results)) for (let V  of sys.$forObject( Row)) {
    const v =sys.$checkNull( V[2]);
    if (sys.asBool(v > MaxMin[0])) MaxMin[0] =sys.$checkExists(MaxMin[0],sys.$checkNull( v));
    if (sys.asBool(v < MaxMin[1])) MaxMin[1] =sys.$checkExists(MaxMin[1],sys.$checkNull( v));
  }

  const max =sys.$checkNull( MaxMin[0]);
  const min =sys.$checkNull( MaxMin[1]);

  const ncols =sys.$checkNull( arr.size(Results[0]));
  const sV =sys.$checkNull( [ParStart[0]]);
  const aV =sys.$checkNull( [ParStart[1]]);
  const sInc =sys.$checkNull( ParStep[0]);
  const aInc =sys.$checkNull( ParStep[1]);
  wg
    .removeAll()
    .add(Q("table")
      .klass("flat")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .klass("rhead")
          .text(II("Start")))
        .adds(arr.fromIter(iter.map(iter.$range(0,ncols), function(i)  {sys.$params(arguments.length, 1);  return Q("td");}))))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Approximation")))
        .add(Q("td"))
        .adds(arr.fromIter(iter.map(iter.$range(1,ncols), function(i)  {sys.$params(arguments.length, 1);
            const r =sys.$checkNull( Q("td")
              .klass("rframe")
              .style("font-size:small")
              .text(math.toIso(aV[0] * 100, 2)));
            aV[0] +=sys.$checkExists(aV[0],sys.$checkNull( aInc));
             return r;
          }))))
      .adds(arr.fromIter(iter.map(iter.$range(1, arr.size(Results)), function(i)  {sys.$params(arguments.length, 1);
          const a2V =sys.$checkNull( [ParStart[1]]);
          const r =sys.$checkNull( Q("tr")
            .add(Q("td"))
            .add(Q("td")
              .klass("rframe")
              .style("font-size:small")
              .text(math.toIso(sV[0] * 100, 2)))
            .adds(arr.fromIter(iter.map(iter.$range(1,ncols), function(j)  {sys.$params(arguments.length, 1);
                const s =sys.$checkNull( sV[0]);
                const a2 =sys.$checkNull(a2V[0]);
                const r =sys.$checkNull( Q("td")
                  .klass("border")
                  .att("title", math.toIso(Results[i][j][2], 0))
                  .style(
                      "cursor:pointer;" +
                      "background:" + color(max, min, Results[i][j][2])
                    )
                  .on("click", function(e)  {sys.$params(arguments.length, 1); window.location.assign(
                      "?" + Url["0"] +
                      "&" + Url["1"] +
                      "&charts" +
                      "&" + math.round(s, 4) +
                      "&" + math.round(a2, 4)
                    );}));
                a2V[0] +=sys.$checkExists(a2V[0],sys.$checkNull( aInc));
                 return r;
              }))));
          sV[0] +=sys.$checkExists(sV[0],sys.$checkNull( sInc));
           return r;
        }))))
    ;
};
