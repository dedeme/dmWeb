import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as lineChart from  "../../libdm/lineChart.js";
import * as cts from  "../../data/cts.js";
import * as profitsEntry from  "../../data/profitsEntry.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "CMarket",
    source: "ProfitsPg",
    rq: "idata"
  }));
  const invs =sys.$checkNull( Rp.investors); 
  const Profits =sys.$checkNull( arr.map(Rp.profits, profitsEntry.fromJs));

  if (sys.asBool(!sys.asBool(Profits))) {
    arr.push(Profits, [new  ProfitsEntry(
      time.toStr(time.mkDate(1, 1, time.year(time.now()))), [0, 0, 0]
    )]);
  }
  if (sys.asBool(sys.$eq(arr.size(Profits) , 1))) arr.push(Profits, Profits[0]);

  

  
   function caption()  {sys.$params(arguments.length, 0);
     return Q("table")
      .klass("frame")
      .att("align", "center")
      .adds(arr.fromIter(iter.map(iter.$range(0,invs), function(i)  {sys.$params(arguments.length, 1);
         return Q("tr")
          .add(Q("td")
            .style("width:30px")
            .add(ui.led(cts.ToSellColors[i], 6)))
          .add(Q("td")
            .style("vertical-align: middle")
            .text(II("Investor") + "-" + i))
        ;})))
    ;};

  if (sys.asBool(sys.$neq(arr.size(Profits[0].Profits) , invs)))
    throw new Error( ("Investor number is not " + invs));

  
  const Labels =sys.$checkNull( []);
  
  const Sets =sys.$checkNull( []);
  for (let i  of sys.$forObject( iter.$range(0,invs))) arr.push(Sets, []);
  for (let E  of sys.$forObject( Profits)) {
    arr.push(Labels, sys.$slice(E.date,6,null) + "/" + sys.$slice(E.date,4,6));
    const Pfs =sys.$checkNull( E.Profits);
    for (let i  of sys.$forObject( iter.$range(0,invs))) arr.push(Sets[i], [Pfs[i]]);
  }
  const SetAtts =sys.$checkNull( [
    lineChart.mkLineExample(),
    lineChart.mkLineExample(),
    lineChart.mkLineExample()
  ]);
  for (let i  of sys.$forObject( iter.$range(0,invs))) SetAtts[i].color =sys.$checkExists(SetAtts[i].color,sys.$checkNull( cts.ToSellColors[i]));
  const Chart =sys.$checkNull( lineChart.mkExample());
  Chart.ExArea.width =sys.$checkExists(Chart.ExArea.width,sys.$checkNull( 600));
  Chart.ExArea.height =sys.$checkExists(Chart.ExArea.height,sys.$checkNull( 400));
  Chart.InPadding =sys.$checkExists(Chart.InPadding,sys.$checkNull( lineChart.mkPadding(25, 25, 30, 100)));

  const Data =sys.$checkNull( lineChart.mkData(Labels, Sets, SetAtts));
  const lenGroup =sys.$checkNull( math.toInt(arr.size(Labels) / 10) + 1);
  Data.drawLabel =sys.$checkExists(Data.drawLabel, function(l, i)  {sys.$params(arguments.length, 2);  return sys.asBool(i > 0) && sys.asBool(sys.$eq(i % lenGroup , 0));});
  Data.drawGrid =sys.$checkExists(Data.drawGrid, function(l, i)  {sys.$params(arguments.length, 2);
     return sys.asBool(sys.asBool(i > 0) && sys.asBool(sys.$eq(i % lenGroup , 0))) && sys.asBool(i < arr.size(Labels) - 1);});

  wg
    .removeAll()
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(caption())))
        .add(Q("tr")
          .add(Q("td")
            .add(lineChart.mkWg(Chart, Data)))))
  ;
};
