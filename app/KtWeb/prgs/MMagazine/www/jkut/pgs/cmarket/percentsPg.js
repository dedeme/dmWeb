import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as lineChart from  "../../libdm/lineChart.js";
import * as cts from  "../../data/cts.js";
import * as profitsEntry from  "../../data/profitsEntry.js";
import * as ibexSundays from  "../../data/ibexSundays.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "CMarket",
    source: "PercentsPg",
    rq: "idata"
  }));
  const invs =sys.$checkNull( Rp.investors); 
  const InitialAssets =sys.$checkNull( Rp.initialAssets); 
  const Profits =sys.$checkNull( arr.map(Rp.profits, profitsEntry.fromJs));
  const ibex =sys.$checkNull( ibexSundays.fromJs(Rp.ibex));

  const pfColor =sys.$checkNull( "#000000");
  const ibexColor =sys.$checkNull( "#800000");

  if (sys.asBool(!sys.asBool(Profits))) {
    arr.push(Profits, [new  ProfitsEntry(
      time.toStr(time.mkDate(1, 1, time.year(time.now()))), [0, 0, 0]
    )]);
  }
  if (sys.asBool(sys.$eq(arr.size(Profits) , 1))) arr.push(Profits, Profits[0]);

  const Ibexdts =sys.$checkNull( ibexSundays.dates(ibex));
  const Ibexrts =sys.$checkNull( ibexSundays.ratios(ibex));
  if (sys.asBool(!sys.asBool(Ibexdts))) {
    arr.push(Ibexdts, time.now());
    arr.push(Ibexrts, 0);
  }
  if (sys.asBool(sys.$eq(arr.size(Ibexdts) , 1))) {
    Ibexdts.push(Ibexdts[0]);
    Ibexrts.push(0);
  }

  

  
   function badData()  {sys.$params(arguments.length, 0);
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .klass("frame")
            .text(II("Dates of profits and ibex does not match"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center;width:50%")
            .klass("border")
            .text(II("Profits")))
          .add(Q("td")
            .style("text-align: center")
            .klass("border")
            .text(II("Ibex"))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center; vertical-align:top")
            .html("<tt>" +
                arr.join(arr.map(Profits, function(P)  {sys.$params(arguments.length, 1);  return P.date;}), "<br>") +
                "<tt>"
              ))
          .add(Q("td")
            .style("text-align:center; vertical-align:top")
            .html("<tt>" +
                arr.join(arr.map(Ibexdts, function(d)  {sys.$params(arguments.length, 1);  return time.toStr(d);}), "<br>") +
                "<tt>"
              ))))
  ;};

  
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
            .style("vertical-align: middle;text-align:left;")
            .text(II("Investor") + "-" + i))
        ;})))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(ui.led(ibexColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text("ibex")))
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(ui.led(pfColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text(II("Investors average"))))
  ;};

  
   function caption2()  {sys.$params(arguments.length, 0);
     return Q("table")
      .klass("frame")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .style("width:30px")
          .add(ui.led(pfColor, 6)))
        .add(Q("td")
          .style("vertical-align: middle;text-align:left;")
          .text(II("Investors - Ibex"))))
  ;};

  if (sys.asBool(sys.$neq(arr.size(Profits[0].Profits) , invs)))
    throw new Error( ("Investor number is not " + invs));

  if (sys.asBool(
    sys.$neq(arr.map(Profits, function(p)  {sys.$params(arguments.length, 1);  return p.date;}) ,
    arr.map(Ibexdts, function(d)  {sys.$params(arguments.length, 1);  return time.toStr(d);})))
  ) {
    badData();
    return;
  }

  const SumInitialAssets =sys.$checkNull( [0]);
  const SumInitialPfs =sys.$checkNull( [0]);
  
  const InitialPfs =sys.$checkNull( []);
  
  const Sums =sys.$checkNull( []);
  
  const Labels =sys.$checkNull( []);
  
  const Sets =sys.$checkNull( []);
  for (let i  of sys.$forObject( iter.$range(0,invs))) {
    const pfs =sys.$checkNull( Profits[0].Profits[i]);
    arr.push(InitialPfs, pfs);
    SumInitialPfs[0] +=sys.$checkExists(SumInitialPfs[0],sys.$checkNull( pfs));
    SumInitialAssets[0] +=sys.$checkExists(SumInitialAssets[0],sys.$checkNull( InitialAssets[i]));
    arr.push(Sets, []);
  }
  for (let E  of sys.$forObject( Profits)) {
    arr.push(Labels, sys.$slice(E.date,6,null) + "/" + sys.$slice(E.date,4,6));
    const Pfs =sys.$checkNull( E.Profits);
    const Sum =sys.$checkNull( [0]);
    for (let i  of sys.$forObject( iter.$range(0,invs))) {
      const dif =sys.$checkNull( Pfs[i] - InitialPfs[i]);
      arr.push(Sets[i], [dif * 100 / InitialAssets[i]]);
      Sum[0] +=sys.$checkExists(Sum[0],sys.$checkNull( dif));
    }
    arr.push(Sums, [Sum[0] * 100 / SumInitialAssets]);
  }
  arr.push(Sets, arr.map(Ibexrts, function(v)  {sys.$params(arguments.length, 1);  return [v * 100];}));
  arr.push(Sets, Sums);

  
  const SetAtts =sys.$checkNull( []);
  for (let i  of sys.$forObject( iter.$range(0,invs))) {
    const Atts =sys.$checkNull( lineChart.mkLineExample());
    Atts.color =sys.$checkExists(Atts.color,sys.$checkNull( cts.ToSellColors[i]));
    arr.push(SetAtts, Atts);
  }
  const IbexAtts =sys.$checkNull( lineChart.mkLineExample());
  IbexAtts.color =sys.$checkExists(IbexAtts.color,sys.$checkNull( ibexColor));
  IbexAtts.width =sys.$checkExists(IbexAtts.width,sys.$checkNull( 3));
  arr.push(SetAtts, IbexAtts);
  const PfAtts =sys.$checkNull( lineChart.mkLineExample());
  PfAtts.color =sys.$checkExists(PfAtts.color,sys.$checkNull( pfColor));
  PfAtts.width =sys.$checkExists(PfAtts.width,sys.$checkNull( 3));
  arr.push(SetAtts, PfAtts);

  const Data =sys.$checkNull( lineChart.mkData(Labels, Sets, SetAtts));
  const lenGroup =sys.$checkNull( math.toInt(arr.size(Labels) / 10) + 1);
  Data.drawLabel =sys.$checkExists(Data.drawLabel, function(l, i)  {sys.$params(arguments.length, 2);  return sys.asBool(i > 0) && sys.asBool(sys.$eq(i % lenGroup , 0));});
  Data.drawGrid =sys.$checkExists(Data.drawGrid, function(l, i)  {sys.$params(arguments.length, 2);
     return sys.asBool(sys.asBool(i > 0) && sys.asBool(sys.$eq(i % lenGroup , 0))) && sys.asBool(i < arr.size(Labels) - 1);});

  const Chart =sys.$checkNull( lineChart.mkExample());
  Chart.ExArea.width =sys.$checkExists(Chart.ExArea.width,sys.$checkNull( 600));
  Chart.ExArea.height =sys.$checkExists(Chart.ExArea.height,sys.$checkNull( 400));
  Chart.InPadding =sys.$checkExists(Chart.InPadding,sys.$checkNull( lineChart.mkPadding(25, 25, 30, 100)));

  const Data2 =sys.$checkNull( lineChart.mkData(
    Labels,
    [arr.fromIter(iter.map(iter.$range(0,arr.size(Sums)), function(i)  {sys.$params(arguments.length, 1);
        const Sum =sys.$checkNull( Sums[i]);
        return sys.asBool( (Sum)) ? [Sum[0] - Ibexrts[i] * 100] : [];
      }))
    ],
    [PfAtts]
  ));
  Data2.drawLabel =sys.$checkExists(Data2.drawLabel,sys.$checkNull( Data.drawLabel));
  Data2.drawGrid =sys.$checkExists(Data2.drawGrid,sys.$checkNull( Data.drawGrid));

  const Chart2 =sys.$checkNull( lineChart.mkExample());
  Chart2.ExArea.width =sys.$checkExists(Chart2.ExArea.width,sys.$checkNull( 600));
  Chart2.ExArea.height =sys.$checkExists(Chart2.ExArea.height,sys.$checkNull( 200));
  Chart2.InPadding =sys.$checkExists(Chart2.InPadding,sys.$checkNull( lineChart.mkPadding(25, 25, 30, 100)));

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(caption())))
      .add(Q("tr")
        .add(Q("td")
          .style("width:5px;")
          .text("%"))
        .add(Q("td")
          .add(lineChart.mkWg(Chart, Data))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .style("height:15px")
          .text(" ")))
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(caption2())))
      .add(Q("tr")
        .add(Q("td")
          .style("width:5px;")
          .text("%"))
        .add(Q("td")
          .add(lineChart.mkWg(Chart2, Data2))))
      )
  ;

};
