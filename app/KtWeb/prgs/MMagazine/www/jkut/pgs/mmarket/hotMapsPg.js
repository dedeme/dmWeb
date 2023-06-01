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
  const MapsGroup =sys.$checkNull( arr.map(Rp.mapsGroup, hotMap.fromJs));

  

  
   function twoChart(Es)  {sys.$params(arguments.length, 1);
    const firstParam =sys.$checkNull( Es[0].Params[0]);
    const cols =sys.$checkNull( arr.size(arr.takeWhile(Es, function(E)  {sys.$params(arguments.length, 1);  return sys.$eq(E.Params[0] , firstParam);})));
    const rows =sys.$checkNull( math.toInt(arr.size(Es) / cols));
    const nfmt0 =sys.$checkNull( fns.paramFormatter(Es[0].Params[0], Es[cols].Params[0]));
    const nfmt1 =sys.$checkNull( fns.paramFormatter(Es[0].Params[1], Es[1].Params[1]));
    const max =sys.$checkNull( arr.reduce(Es, Es[0].ev, function(r, E)  {sys.$params(arguments.length, 2); return sys.asBool( E.ev > r) ? E.ev : r;}));
    const min =sys.$checkNull( arr.reduce(Es, Es[0].ev, function(r, E)  {sys.$params(arguments.length, 2); return sys.asBool( E.ev < r) ? E.ev : r;}));
    const color =sys.$checkNull( fns.valueColor(max, min));

     return Q("table")
      .klass("flat")
      .adds(arr.fromIter(iter.map(iter.$range(0,rows), function(row)  {sys.$params(arguments.length, 1);  return Q("tr")
          .adds(arr.fromIter(iter.map(iter.$range(0,cols), function(col)   {sys.$params(arguments.length, 1);
              const i =sys.$checkNull( row * cols + col);
              const E =sys.$checkNull( Es[i]);
               return Q("td")
                .style(
                    "padding:0px;" +
                    "width: 6px; height: 6px;" +
                    "background: " + color(E.ev)
                  )
                .att(
                    "title",
                    nfmt0(E.Params[0]) + " - " +
                    nfmt1(E.Params[1]) + "\n" +
                    math.toIso(E.ev / 100, 2)
                  )
              ;
            })))
        ;})))
    ;
  };

  
   function oneChart(Es)  {sys.$params(arguments.length, 1);
    const nfmt =sys.$checkNull( fns.paramFormatter(Es[0].Params[0], Es[1].Params[0]));
    const max =sys.$checkNull( arr.reduce(Es, Es[0].ev, function(r, E)  {sys.$params(arguments.length, 2); return sys.asBool( E.ev > r) ? E.ev : r;}));
    const min =sys.$checkNull( arr.reduce(Es, Es[0].ev, function(r, E)  {sys.$params(arguments.length, 2); return sys.asBool( E.ev < r) ? E.ev : r;}));
    const color =sys.$checkNull( fns.valueColor(max, min));

     return Q("table")
      .klass("flat")
      .adds(arr.map(Es, function(E)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .style(
              "padding:0px;" +
              "width: 120px; height: 6px;" +
              "background: " + color(E.ev)
            )
          .att(
              "title",
              nfmt(E.Params[0]) + "\n" +
              math.toIso(E.ev / 100, 2)
            ));}))
      ;
  };

  
   function mapChart(Map)  {sys.$params(arguments.length, 1);
     return Q("table")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("text-align:center")
          .text(time.toIso(time.fromStr(Map.date)))))
      .add(Q("tr")
        .add(Q("td")
          .add(sys.asBool(sys.$eq(arr.size(Map.Entries[0].Params) , 1))
            ? oneChart(Map.Entries)
            : twoChart(Map.Entries))))
    ;};

  
   function rowGroups(start, end)  {sys.$params(arguments.length, 2);
     return Q("table")
      .att("align", "center")
      .add(Q("tr")
        .adds(arr.fromIter(iter.map(iter.$range(start,end),  function(i)  {sys.$params(arguments.length, 1);
           return Q("td").add(mapChart(MapsGroup[i]));}
        ))))
    ;};

  const Lopts =sys.$checkNull( []);
  for (let M  of sys.$forObject( Models)) {
    arr.push(Lopts, menu.separator());
    arr.push(Lopts, menu.tlink(M, M, ["mmarket&hotmaps"]));
  }
  arr.shift(Lopts);
  const menuWg =sys.$checkNull( menu.mk(Lopts, [], modelSel, false));


  const groups =sys.$checkNull( arr.size(MapsGroup));
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
