import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as cts from  "../../data/cts.js";
import * as fns from  "../../data/fns.js";
import * as hotMap from  "../../data/hotMap.js";
import * as paramsEval from  "../../data/paramsEval.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Url =sys.$checkNull( ui.url());
  const model =sys.$checkNull(sys.asBool( dic.hasKey(Url, "2")) ? Url["2"] : "");
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "MMarket",
    source: "HotMapsPg",
    rq: "idata",
    model:model
  }));
  const modelSel =sys.$checkNull( Rp.model);
  
  const Models =sys.$checkNull( Rp.models);
  arr.sort(Models, function(m1, m2)  {sys.$params(arguments.length, 2);  return m1 < m2;});
  
  const DateGroups =sys.$checkNull( Rp.datesGroup);
  arr.sort(DateGroups, function(d1, d2)  {sys.$params(arguments.length, 2);  return d1 > d2;});

  

  
   function twoChart(PsEvs)  {sys.$params(arguments.length, 1);
    const firstParam =sys.$checkNull( PsEvs[0][0][0]);
    const cols =sys.$checkNull( arr.size(arr.takeWhile(PsEvs, function(PsE)  {sys.$params(arguments.length, 1);  return sys.$eq(PsE[0][0] , firstParam);})));
    const rows =sys.$checkNull( math.toInt(arr.size(PsEvs) / cols));
    const nfmt0 =sys.$checkNull( fns.paramFormatter(PsEvs[0][0][0], PsEvs[cols][0][0]));
    const nfmt1 =sys.$checkNull( fns.paramFormatter(PsEvs[0][0][1], PsEvs[1][0][1]));
    const max =sys.$checkNull( arr.reduce(
      PsEvs, PsEvs[0][1], function(r, PsE)  {sys.$params(arguments.length, 2); return sys.asBool( PsE[1] > r) ? PsE[1] : r;}
    ));
    const min =sys.$checkNull( arr.reduce(
      PsEvs, PsEvs[0][1], function(r, PsE)  {sys.$params(arguments.length, 2); return sys.asBool( PsE[1] < r) ? PsE[1] : r;}
    ));
    const color =sys.$checkNull( fns.valueColor(max, min));

     return Q("table")
      .klass("flat")
      .adds(arr.fromIter(iter.map(iter.$range(0,rows), function(row)  {sys.$params(arguments.length, 1);  return Q("tr")
          .adds(arr.fromIter(iter.map(iter.$range(0,cols), function(col)   {sys.$params(arguments.length, 1);
              const i =sys.$checkNull( row * cols + col);
              const PsE =sys.$checkNull( PsEvs[i]);
               return Q("td")
                .style(
                    "padding:0px;" +
                    "width: 6px; height: 6px;" +
                    "background: " + color(PsE[1])
                  )
                .att(
                    "title",
                    nfmt0(PsE[0][0]) + " - " +
                    nfmt1(PsE[0][1]) + "\n" +
                    math.toIso(PsE[1], 0)
                  )
              ;
            })))
        ;})))
    ;
  };

  
   function oneChart(PsEvs)  {sys.$params(arguments.length, 1);
    const nfmt =sys.$checkNull( fns.paramFormatter(PsEvs[0][0][0], PsEvs[1][0][0]));
    const max =sys.$checkNull( arr.reduce(
      PsEvs, PsEvs[0][1], function(r, PsE)  {sys.$params(arguments.length, 2); return sys.asBool( PsE[1] > r) ? PsE[1] : r;}
    ));
    const min =sys.$checkNull( arr.reduce(
      PsEvs, PsEvs[0][1], function(r, PsE)  {sys.$params(arguments.length, 2); return sys.asBool( PsE[1] < r) ? PsE[1] : r;}
    ));
    const color =sys.$checkNull( fns.valueColor(max, min));

     return Q("table")
      .klass("flat")
      .adds(arr.map(PsEvs, function(PsE)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .style(
              "padding:0px;" +
              "width: 120px; height: 6px;" +
              "background: " + color(PsE[1])
            )
          .att(
              "title",
              nfmt(PsE[0][0]) + "\n" +
              math.toIso(PsE[1], 0)
            ));}))
      ;
  };

  
   async  function mapChart(td, date)  {sys.$params(arguments.length, 2);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "MMarket",
      source: "HotMapsPg",
      rq: "evals",
      modelSel:modelSel,
      date:date
    }));
    const ParamsEvals =sys.$checkNull( Rp.paramsEvals); 

    td.add(
      Q("table")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("text-align:center")
          .text(time.toIso(time.fromStr(date)))))
      .add(Q("tr")
        .add(Q("td")
          .add(sys.asBool(sys.$eq(arr.size(ParamsEvals[0][0]) , 1))
            ? oneChart(ParamsEvals)
            : twoChart(ParamsEvals))))
    );
  };

  
   function rowGroups(start, end)  {sys.$params(arguments.length, 2);
    const Tds =sys.$checkNull( []);
    for (let i = start;i < end; ++i) arr.push(Tds, Q("td"));
    for (let i = start;i < end; ++i) mapChart(Tds[i], DateGroups[i]);
     return Q("table")
      .att("align", "center")
      .add(Q("tr").adds(Tds))
    ;
  };

  const Lopts =sys.$checkNull( []);
  for (let M  of sys.$forObject( Models)) {
    arr.push(Lopts, menu.separator());
    arr.push(Lopts, menu.tlink(M, M, ["mmarket&hotmaps"]));
  }
  arr.shift(Lopts);
  const menuWg =sys.$checkNull( menu.mk(Lopts, [], modelSel, false));


  const groups =sys.$checkNull( arr.size(DateGroups));
  wg
    .removeAll()
    .add(menuWg)
    .add(rowGroups(0,sys.asBool( groups >= 4) ? 4 : groups))
  ;

  if (sys.asBool(groups >= 4))
    wg
      .add(rowGroups(4,sys.asBool( groups >= 8) ? 8 : groups))
    ;

  if (sys.asBool(groups >= 8))
    wg
      .add(Q("hr"))
      .add(rowGroups(8,sys.asBool( groups >= 13) ? 13 : groups))
    ;

  if (sys.asBool(groups >= 13))
    wg
      .add(rowGroups(13,sys.asBool( groups >= 18) ? 18 : groups))
    ;

  if (sys.asBool(groups >= 18))
    wg
      .add(Q("hr"))
      .add(rowGroups(18, groups))
    ;

};
