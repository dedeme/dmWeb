import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';




import * as menu from  "./libdm/menu.js";
import * as cts from  "./data/cts.js";
import * as msgPg from  "./pgs/msgPg.js";
import * as listPg from  "./pgs/listPg.js";
import * as hotMapPg from  "./pgs/hotMapPg.js";
import * as rankingsPg from  "./pgs/rankingsPg.js";
import * as chartsPg from  "./pgs/chartsPg.js";
import * as i18n from  "./i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 async  function mk(wg)  {sys.$params(arguments.length, 1);
  const ok =sys.$checkNull( await  client.connect());
  if (sys.asBool(!sys.asBool(ok))) {
    ui.alert(II("KtWeb session is closed.\nAuthenticating from KtWeb:Main."));
    window.location.assign("http://" + window.location.host + "/Main");
    return;
  }

  const Rp0 =sys.$checkNull( await  client.send({
    prg: "Main", 
    source: "Main",
    rq: "lang"
  }));
  if (sys.asBool(sys.$eq(Rp0.lang , "en"))) i18n.en();

  const Url =sys.$checkNull( ui.url());
  const daysWinOp =sys.$checkNull( dic.get(Url, "0"));
  if (sys.asBool(!sys.asBool(daysWinOp))) {
    window.location.assign("?" + cts.daysWinDefault);
    return;
  }

  const daysLossOp =sys.$checkNull(sys.asBool( daysWinOp) ? dic.get(Url, "1") : []);
  if (sys.asBool(!sys.asBool(daysLossOp))) {
    window.location.assign("?" + daysWinOp[0] + "&" + cts.daysLossDefault);
    return;
  }

  const days =sys.$checkNull(sys.asBool( sys.$eq(sys.$slice(daysWinOp[0],1,null) , "ranking"))
      ? [math.toStr(cts.daysWinDefault), daysLossOp[0]]
      : [daysWinOp[0], daysLossOp[0]])
  ;
  const sel3V =sys.$checkNull( dic.get(Url, "2"));
  const sel3 =sys.$checkNull(sys.asBool( sys.asBool(sel3V) &&
        sys.asBool(arr.any(
          ["nlst", "nhot", "flst", "fhot", "charts"],
          function(s)  {sys.$params(arguments.length, 1);  return sys.$eq(s , sel3V[0]);}
        )))
    ? sel3V[0]
    : "nlst")
  ;

  const Rp =sys.$checkNull( await  client.send({
    prg: "DeepMarket",
    source: "Main",
    rq: "idata",
    days:days
  }));
  const daysWin =sys.$checkNull( Rp.daysWin); 
  const daysLoss =sys.$checkNull( Rp.daysLoss); 

  if (sys.asBool(sys.$neq([daysWin, daysLoss] , days))) {
    window.location.assign("?" + daysWin + "&" + daysLoss);
    return;
  }

  const sel1 =sys.$checkNull(sys.asBool( sys.asBool(daysWinOp) && sys.asBool(sys.$eq(sys.$slice(daysWinOp[0],1,null) , "ranking"))) ? daysWinOp[0] : daysWin);

  const Lopts1 =sys.$checkNull( []); 
  for (let d  of sys.$forObject( cts.DaysWin)) {
    arr.push(Lopts1, menu.tlink(d + "&" + daysLoss + "&" + sel3, d, []));
    arr.push(Lopts1, menu.separator());
  }
  arr.pop(Lopts1);
  const Ropts1 =sys.$checkNull( [
    menu.tlink("nranking&" + daysLoss + "&" + sel3, II("Near - Ranking"), []),
    menu.separator(),
    menu.tlink("franking&" + daysLoss + "&" + sel3, II("Far - Ranking"), [])
  ]);
  const menuWg1 =sys.$checkNull( menu.mk(Lopts1, Ropts1, sel1 + "&" + daysLoss + "&" + sel3, false));

  if (sys.asBool(sys.asBool(daysWinOp) && sys.asBool(sys.$eq(sys.$slice(daysWinOp[0],1,null) , "ranking")))) {
    const body =sys.$checkNull( Q("div"));
    rankingsPg.mk(body, sys.$eq(daysWinOp[0] , "nranking"));
    wg
      .removeAll()
      .add(menuWg1)
      .add(body)
    ;
    return;
  }

  const Lopts2 =sys.$checkNull( []); 
  for (let d  of sys.$forObject( cts.DaysLoss)) {
    arr.push(Lopts2, menu.tlink(sel1 + "&" + d + "&" + sel3, d, []));
    arr.push(Lopts2, menu.separator());
  }
  arr.pop(Lopts2);
  const menuWg2 =sys.$checkNull( menu.mk(Lopts2, [], daysWin + "&" + daysLoss + "&" + sel3, false));

  const Lopts3 =sys.$checkNull( [
    menu.tlink(daysWin + "&" + daysLoss + "&nlst", II("[Near - List]"), []),
    menu.separator(),
    menu.tlink(daysWin + "&" + daysLoss + "&nhot", II("[Near - Hot Map]"), []),
    menu.separator2(),
    menu.tlink(daysWin + "&" + daysLoss + "&flst", II("[Far - List]"), []),
    menu.separator(),
    menu.tlink(daysWin + "&" + daysLoss + "&fhot", II("[Far - Hot Map]"), [])
  ]);
  const Ropts3 =sys.$checkNull( [
    menu.tlink(daysWin + "&" + daysLoss + "&charts", II("Charts"), [])
  ]);
  const menuWg3 =sys.$checkNull( menu.mk(Lopts3, Ropts3, daysWin + "&" + daysLoss + "&" + sel3, false));

  const body =sys.$checkNull( Q("div"));

  wg
    .removeAll()
    .add(menuWg1)
    .add(menuWg2)
    .add(menuWg3)
    .add(body)
  ;

  if (sys.asBool(sys.$eq(sel3 , "charts"))) {
    chartsPg.mk(body);
  } else {
    const isNear =sys.$checkNull( sys.asBool(sys.$eq(sel3 , "nlst")) || sys.asBool(sys.$eq(sel3 , "nhot")));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Main",
      rq: "results",
      days: "[" + daysWin + "-" + daysLoss + "]",
      isNear:isNear
    }));
    const ParNearStart =sys.$checkNull( Rp.parNearStart); 
    const ParFarStart =sys.$checkNull( Rp.parFarStart); 
    const ParStep =sys.$checkNull( Rp.parStep); 
    const Results =sys.$checkNull( Rp.results); 
                          
                          
    switch (sel3) {
      case "nhot":{ hotMapPg.mk(body, ParNearStart, ParStep, Results);break;}
      case "fhot":{ hotMapPg.mk(body, ParFarStart, ParStep, Results);break;}
      default:{ listPg.mk(
        body, isNear, ParNearStart, ParFarStart, ParStep, Results
      );}
    }
  }
};



const wg =sys.$checkNull( Q("div"));


export  function load()  {sys.$params(arguments.length, 0);
  mk(wg);
};

client.init(true, "KtWeb", function()  {sys.$params(arguments.length, 0);
  const msgWg =sys.$checkNull( Q("div"));
  msgPg.mk(msgWg, II("Session is expired."), true);
  Q("@body")
    .removeAll()
    .add(msgWg)
    .add(cts.foot)
  ;
});

Q("@body")
  .removeAll()
  .add(wg)
  .add(cts.foot)
  .add(ui.upTop("up"))
;

load();
