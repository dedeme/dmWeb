import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as lineChart from  "../../libdm/lineChart.js";
import * as cts from  "../../data/cts.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg, daysWin, daysLoss, Params)  {sys.$params(arguments.length, 4);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "HistoricPg",
    rq: "idata",
    daysWin:daysWin,
    daysLoss:daysLoss,
    params: Params
  }));
  const Dates =sys.$checkNull( Rp.dates); 
  const Assets =sys.$checkNull( Rp.assets); 
  const Withdrawals =sys.$checkNull( Rp.withdrawals); 

  
   function mkGr(isAssets)  {sys.$params(arguments.length, 1);
    const Labels =sys.$checkNull( arr.map(Dates, function(d)  {sys.$params(arguments.length, 1);  return sys.$slice(d,4,6);}));

    const Ch =sys.$checkNull( lineChart.mkExample());
    Ch.ExArea.width =sys.$checkExists(Ch.ExArea.width,sys.$checkNull( 600));
    Ch.ExArea.height =sys.$checkExists(Ch.ExArea.height,sys.$checkNull(sys.asBool( isAssets) ? 300 : 150));
    Ch.InPadding.left =sys.$checkExists(Ch.InPadding.left,sys.$checkNull( 100));
    Ch.ExArea.Atts.background =sys.$checkExists(Ch.ExArea.Atts.background,sys.$checkNull( "#ffffff"));
    Ch.InAtts.background =sys.$checkExists(Ch.InAtts.background,sys.$checkNull( "#e9e9e9"));

    const Data =sys.$checkNull( lineChart.mkData(
      Labels,sys.asBool(
      isAssets)
        ? [arr.map(Assets, function(e) {sys.$params(arguments.length, 1); return [e];})]
        : [arr.map(Withdrawals, function(e) {sys.$params(arguments.length, 1); return [e];})],
      [ lineChart.mkLine(1, "#000000", false)]
    ));
    Data.round =sys.$checkExists(Data.round,sys.$checkNull( 0));
    const PrevLabel =sys.$checkNull( [Labels[0]]);
    Data.drawLabel =sys.$checkExists(Data.drawLabel, function(l, i)  {sys.$params(arguments.length, 2);
      if (sys.asBool(sys.$eq(i , 0)))  return false;
      if (sys.asBool(sys.asBool(sys.$neq(l , PrevLabel[0])) && sys.asBool((sys.asBool(sys.asBool(sys.asBool(sys.$eq(l , "01")) || sys.asBool(sys.$eq(l , "04"))) || sys.asBool(sys.$eq(l , "07")))|| sys.asBool(sys.$eq(l , "10")))))) {
        PrevLabel[0] =sys.$checkExists(PrevLabel[0],sys.$checkNull( l));
         return true;
      }
       return false;
    });
    const PrevLabel2 =sys.$checkNull( [Labels[0]]);
    Data.drawGrid =sys.$checkExists(Data.drawGrid, function(l, i)  {sys.$params(arguments.length, 2);
      if (sys.asBool(sys.$eq(i , 0)))  return false;
      if (sys.asBool(sys.asBool(sys.$neq(l , PrevLabel2[0])) && sys.asBool((sys.asBool(sys.asBool(sys.asBool(sys.$eq(l , "01")) || sys.asBool(sys.$eq(l , "04"))) || sys.asBool(sys.$eq(l , "07")))|| sys.asBool(sys.$eq(l , "10")))))) {
        PrevLabel2[0] =sys.$checkExists(PrevLabel2[0],sys.$checkNull( l));
         return true;
      }
       return false;
    });

     return lineChart.mkWg(Ch, Data);
  };

  wg
    .removeAll()
    .add(Q("div")
      .klass("head")
      .text(II("Assets")))
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(mkGr(true)))))
    .add(Q("div")
      .klass("head")
      .text(II("Withdrawals")))
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(mkGr(false)))))
  ;
};
