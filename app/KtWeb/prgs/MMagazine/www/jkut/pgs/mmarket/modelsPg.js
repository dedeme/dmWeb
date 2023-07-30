import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as cts from  "../../data/cts.js";
import * as modelFloats from  "../../data/modelFloats.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Url =sys.$checkNull( ui.url());
  const type0 =sys.$checkNull(sys.asBool( dic.hasKey(Url, "2")) ? Url["2"] : "");
  const type =sys.$checkNull(sys.asBool( sys.asBool(sys.$neq(type0 , "assets")) && sys.asBool(sys.$neq(type0 , "profits"))) ? "points" : type0);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "MMarket",
    source: "ModelsPg",
    rq: "idata"
  }));
  
  const DateGroups =sys.$checkNull( Rp.dateGroups);
  arr.sort(DateGroups, function(d1, d2)  {sys.$params(arguments.length, 2);  return d1 > d2;});

  

  
   async  function groupTable(td, date)  {sys.$params(arguments.length, 2);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "MMarket",
      source: "ModelsPg",
      rq: "rank",
      date:date,
      type:type
    }));
    const ModelValues =sys.$checkNull( Rp.modelValues);
    arr.sort(ModelValues, function(Mv1, Mv2)  {sys.$params(arguments.length, 2);  return Mv1[1] > Mv2[1];});

  td.add(
    Q("table")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .att("colspan", 2)
          .style("text-align:center")
          .text(time.toIso(time.fromStr(date)))))
      .add(Q("tr")
        .add(Q("td")
          .klass("lhead")
          .text(II("Model")))
        .add(Q("td")
          .klass("rhead")
          .text(sys.asBool(sys.$eq(type , "points"))
              ? II("Points")
              :sys.asBool( sys.$eq(type , "profits"))
                ? "%"
                : "€"
            )))
      .adds(arr.map(ModelValues, function(Mv)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .klass("lframe")
          .text(Mv[0]))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(
              Mv[1],sys.asBool(
              sys.$eq(type , "points"))
                ? 0
                :sys.asBool( sys.$eq(type , "profits"))
                  ? 4
                  : 0
            )));}))
    );
  };

  
   function rowGroups(start, end)  {sys.$params(arguments.length, 2);
    const Tds =sys.$checkNull( []);
    for (let i = start;i < end; ++i) arr.push(Tds, Q("td"));
    for (let i = start;i < end; ++i) groupTable(Tds[i], DateGroups[i]);
     return Q("table")
      .att("align", "center")
      .add(Q("tr").adds(Tds))
    ;
  };

  const lopts =sys.$checkNull( [
    menu.tlink("points", II("Points"), ["mmarket&models"]),
    menu.separator(),
    menu.tlink("assets", II("Assets"), ["mmarket&models"]),
    menu.separator(),
    menu.tlink("profits", II("Profits"), ["mmarket&models"])
  ]);
  const menuWg =sys.$checkNull( menu.mk(lopts, [], type, false));


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
