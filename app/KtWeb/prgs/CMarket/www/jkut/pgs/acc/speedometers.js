import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as speedometer from  "../../libdm/speedometer.js";
import * as cts from  "../../data/cts.js";
import * as profitsEntry from  "../../data/chart/profitsEntry.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);



 function acumulatorAvg(value, price)  {sys.$params(arguments.length, 2);
  const profits =sys.$checkNull( value - price);
  const rV =sys.$checkNull( [0.5 - profits / (2 * profits + 4 * price)]);
  if (sys.asBool(rV[0] > 0.5)) {
    rV[0] =sys.$checkExists(rV[0],sys.$checkNull( 0.5 + (rV[0] - 0.5) * 4));
    if (sys.asBool(rV[0] > 1)) rV[0] =sys.$checkExists(rV[0],sys.$checkNull( 1));
  } else if (sys.asBool(rV[0] < 0.5)) {
    rV[0] =sys.$checkExists(rV[0],sys.$checkNull( 0.5 - (0.5 - rV[0]) * 4));
    if (sys.asBool(rV[0] < 0)) rV[0] =sys.$checkExists(rV[0],sys.$checkNull( 0));
  }
   return rV[0];
};


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Acc",
    source: "Speedometers",
    rq: "idata"
  }));

  const Cos =sys.$checkNull( dic.toArr(Rp.cos)); 
  const cash =sys.$checkNull( Rp.cash);
  const equity =sys.$checkNull(  -Rp.equity);

  const Stocks =sys.$checkNull( arr.reduce(
    Cos,
    [0, 0, 0],
    function(R, C)  {sys.$params(arguments.length, 2);
      const Vs =sys.$checkNull( C[1]);
       return [R[0] + Vs[0], R[1] + Vs[1], R[2] + Vs[2]];
    }
  ));
  const prices =sys.$checkNull( Stocks[0]);
  const values =sys.$checkNull( Stocks[1]);
  const refs =sys.$checkNull( Stocks[2]);

  const assets =sys.$checkNull( values + cash);
  const rassets =sys.$checkNull( refs + cash);

  const orderV =sys.$checkNull( ["+nick"]);

  const Show =sys.$checkNull( [[]]);

  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    arr.sort(
      Cos,
      function(C1, C2)  {sys.$params(arguments.length, 2);
        const nk1 =sys.$checkNull( C1[0]);
        const vs1 =sys.$checkNull( C1[1]);
        const nk2 =sys.$checkNull( C2[0]);
        const vs2 =sys.$checkNull( C2[1]);
           
          return sys.$eq(orderV[0],"+profits")?
            acumulatorAvg(vs1[1], vs1[0]) < acumulatorAvg(vs2[1], vs2[0]):
          sys.$eq(orderV[0],"-profits")?
            acumulatorAvg(vs1[1], vs1[0]) > acumulatorAvg(vs2[1], vs2[0]):
          sys.$eq(orderV[0],"+ref")?
            acumulatorAvg(vs1[2], vs1[0]) < acumulatorAvg(vs2[2], vs2[0]):
          sys.$eq(orderV[0],"-ref")?
            acumulatorAvg(vs1[2], vs1[0]) > acumulatorAvg(vs2[2], vs2[0]):
          sys.$eq(orderV[0],"-nick")?
            nk1 > nk2:
          
            nk1 < nk2
        ;
      }
    );

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(II("Global")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(II("Investment")))
          .add(Q("td")
            .klass("header")
            .text(II("Profits")))
          .add(Q("td")
            .klass("header")
            .text(II("Risk"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("number")
            .text(math.toIso(equity, 2)))
          .add(Q("td")
            .klass("frame")
            .add(speedometer.mk(
                acumulatorAvg(assets, equity),
                0.4,
                [], []
              )))
          .add(Q("td")
            .klass("frame")
            .add(speedometer.mk(
                acumulatorAvg(rassets, equity),
                0.4,
                [], []
              )))))
      .add(Q("div")
        .klass("head")
        .text(II("Stocks")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(II("Investment")))
          .add(Q("td")
            .klass("header")
            .text(II("Profits")))
          .add(Q("td")
            .klass("header")
            .text(II("Risk"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("number")
            .text(math.toIso(prices, 2)))
          .add(Q("td")
            .klass("frame")
            .add(speedometer.mk(
                acumulatorAvg(values, prices),
                0.4,
                [], []
              )))
          .add(Q("td")
            .klass("frame")
            .add(speedometer.mk(
                acumulatorAvg(refs, prices),
                0.4,
                [], []
              )))))
      .add(Q("div")
        .klass("head")
        .text(II("Companies")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1);
                  orderV[0] =sys.$checkExists(orderV[0],sys.$checkNull(sys.asBool( sys.$eq(orderV[0] , "+nick")) ? "-nick" : "+nick"));
                  Show[0]();
                })
              .klass("link")
              .text("Nick")))
          .add(Q("td")
            .klass("header")
            .text(II("Investment")))
          .add(Q("td")
            .klass("header")
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1);
                  orderV[0] =sys.$checkExists(orderV[0],sys.$checkNull(sys.asBool( sys.$eq(orderV[0] , "+profits")) ? "-profits" : "+profits"));
                  Show[0]();
                })
              .klass("link")
              .text(II("Profits"))))
          .add(Q("td")
            .klass("header")
            .add(ui.link(function(ev)  {sys.$params(arguments.length, 1);
                  orderV[0] =sys.$checkExists(orderV[0],sys.$checkNull(sys.asBool( sys.$eq(orderV[0] , "+ref")) ? "-ref" : "+ref"));
                  Show[0]();
                })
              .klass("link")
              .text(II("Risk")))))
        .adds(arr.map(Cos, function(C)  {sys.$params(arguments.length, 1);
            const nick =sys.$checkNull( C[0]);
            const Vs =sys.$checkNull( C[1]);
             return Q("tr")
              .add(Q("td")
                .klass("border")
                .text(nick))
              .add(Q("td")
                .klass("number")
                .text(math.toIso(Vs[0], 2)))
              .add(Q("td")
                .klass("frame")
                .add(speedometer.mk(
                    acumulatorAvg(Vs[1], Vs[0]),
                    0.25,
                    [], []
                  )))
              .add(Q("td")
                .klass("frame")
                .add(speedometer.mk(
                    acumulatorAvg(Vs[2], Vs[0]),
                    0.25,
                    [], []
                  )))
            ;
          })))
    ;
  });

  Show[0]();
};
