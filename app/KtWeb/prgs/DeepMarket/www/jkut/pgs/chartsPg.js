import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as cosPg from  "../pgs/charts/cosPg.js";
import * as historicPg from  "../pgs/charts/historicPg.js";
import * as operationsPg from  "../pgs/charts/operationsPg.js";
import * as cts from  "../data/cts.js";
import * as result from  "../data/result.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Url =sys.$checkNull( ui.url());
  try{ {
    const daysWinOp =sys.$checkNull( dic.get(Url, "0"));
    const daysLossOp =sys.$checkNull( dic.get(Url, "1"));
    const selOp =sys.$checkNull( dic.get(Url, "2"));
    if (sys.asBool(
      sys.asBool(sys.asBool(sys.asBool(sys.asBool(sys.asBool(!sys.asBool(daysWinOp)) || sys.asBool(!sys.asBool(daysLossOp))) || sys.asBool(!sys.asBool(selOp))) ||
      sys.asBool(!sys.asBool(math.isDigits(daysWinOp[0])))) ||
      sys.asBool(!sys.asBool(math.isDigits(daysLossOp[0])))) ||
      sys.asBool(sys.$neq(selOp[0] , "charts")))
    )
      throw new Error( II("Malformed URL"));
    const daysWin =sys.$checkNull( math.fromStr(daysWinOp[0])[0]);
    const daysLoss =sys.$checkNull( math.fromStr(daysLossOp[0])[0]);
    if (sys.asBool(!sys.asBool(arr.any(cts.DaysWin, function(i)  {sys.$params(arguments.length, 1);  return sys.$eq(i , daysWin);}))))
      throw new Error( i18n.fmt(II("Value '%0' for 'days win' is not valid"), ["" + daysWin]));
    if (sys.asBool(!sys.asBool(arr.any(cts.DaysLoss, function(i)  {sys.$params(arguments.length, 1);  return sys.$eq(i , daysLoss);}))))
      throw new Error( i18n.fmt(II("Value '%0' for 'days loss' is not valid"), ["" + daysLoss]));
  }} catch (e){ {
    ui.alert(e);
    window.location.assign("?");
    return;
  }}

  const daysWin =sys.$checkNull( math.fromStr(Url["0"])[0]);
  const daysLoss =sys.$checkNull( math.fromStr(Url["1"])[0]);

  
  const PsOp =sys.$checkNull( []);
  if (sys.asBool(dic.get(Url, "3"))) {
    try{ {
      if (sys.asBool(!sys.asBool(dic.get(Url, "4")))) throw new Error( II("Malformed URL"));
      if (sys.asBool(sys.asBool(!sys.asBool(math.fromStr(Url["3"]))) || sys.asBool(!sys.asBool(math.fromStr(Url["4"])))))
      throw new Error( i18n.fmt(
        II("Value [%0,%1] for parameters is not valid"),
        [Url["3"], Url["4"]]
      ));
      arr.push(PsOp, [math.fromStr(Url["3"])[0], math.fromStr(Url["4"])[0]]);
    }} catch (e){ {
      ui.alert(e);
      window.location.assign("?");
      return;
    }}
  }

  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "ChartsPg",
    rq: "idata",
    daysWin:daysWin,
    daysLoss:daysLoss,
    psOp: PsOp 
  }));
  const Params =sys.$checkNull( Rp.params);
  const Result =sys.$checkNull( result.fromJs(Rp.result));
  const Cos =sys.$checkNull( Rp.cos); 


  const Chart =sys.$checkNull( ["cos"]);
  const Show =sys.$checkNull( [[]]);



  
   function go(chart)  {sys.$params(arguments.length, 1);
    Chart[0] =sys.$checkExists(Chart[0],sys.$checkNull( chart));
    Show[0]();
  };



  
  Show[0] =sys.$checkExists(Show[0], async  function()  {sys.$params(arguments.length, 0);
    const Lopts =sys.$checkNull( [
      menu.toption("cos", II("Companies"), function()  {sys.$params(arguments.length, 0); go("cos");}),
      menu.separator(),
      menu.toption("historic", II("Historic"), function()  {sys.$params(arguments.length, 0); go("historic");}),
      menu.separator(),
      menu.toption("operations", II("Operations"), function()  {sys.$params(arguments.length, 0); go("operations");})
    ]);
    const menuWg =sys.$checkNull( menu.mk(Lopts, [], Chart[0], false));

    const body1 =sys.$checkNull( Q("div")
      .add(Q("div")
        .klass("head")
        .text(II("Companies")))
      .add(Q("div").klass("separator"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(II("Start")))
          .add(Q("td")
            .klass("rhead")
            .text(II("Approximation"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Params[0] * 100, 2) + "%"))
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Params[1] * 100, 2) + "%"))))
      .add(Q("div").klass("separator2"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(II("Assets")))
          .add(Q("td")
            .klass("rhead")
            .text(II("Profits (%)")))
          .add(Q("td")
            .klass("rhead")
            .text(II("Eval.")))
          .add(Q("td")
            .klass("rhead")
            .text(II("Sales"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Result.assets, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Result.profits * 100, 2)))
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Result.points, 0)))
          .add(Q("td")
            .klass("rframe")
            .text(math.toIso(Result.sales, 0)))))
      .add(Q("div").klass("separator")))
    ;

    const body2 =sys.$checkNull( Q("div"));
    switch (Chart[0]) {
      case "historic":{ historicPg.mk(body2, daysWin, daysLoss, Params);break;}
      case "operations":{ operationsPg.mk(body2, daysWin, daysLoss, Params);break;}
      default:{ cosPg.mk(body2, daysWin, daysLoss, Params, Cos);}
    }

    wg
      .removeAll()
      .add(menuWg)
      .add(body1)
      .add(body2)
    ;
  });

  Show[0]();
};
