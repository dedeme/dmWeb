import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as model from  "../../data/model.js";
import * as result from  "../../data/result.js";
import * as modelEval from  "../../data/modelEval.js";
import * as order from  "../../data/order.js";
import * as cts from  "../../data/cts.js";
import * as broker from  "../../data/broker.js";
import * as i18n from  "../../i18n.js";
import * as fns from  "../../fns.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg, modelId)  {sys.$params(arguments.length, 2);
  
  const Params =sys.$checkNull( []);
  const Url =sys.$checkNull( ui.url());
  const Uparams =sys.$checkNull( dic.get(Url, "2"));
  if (sys.asBool(Uparams)) {
    try{ {
      const A =sys.$checkNull( js.r(Uparams[0]));
      const ok =sys.$checkNull( arr.reduce(
        A, true, function(r, e)  {sys.$params(arguments.length, 2);  return sys.asBool(sys.asBool(r) && sys.asBool(sys.$eq(sys.type(e) , "number"))) && sys.asBool(e >= 0);}
      ));
      if (sys.asBool(ok)) arr.push(Params, A);
    }} catch (e){ {}}
  }

  const Rp =sys.$checkNull( await  client.send({
    prg: "CMMarket",
    source: "OperationsPg",
    rq: "idata",
    modelId:modelId,
    params: Params 
  }));
  if (sys.asBool(!sys.asBool(Rp.ok))) {
    ui.alert(i18n.fmt(II("%0%1 not found."), [modelId, sys.toStr(Params)]));
    window.location.assign("?");
    return;
  }
  const Model =sys.$checkNull( model.fromJs(Rp.model));
  const Result =sys.$checkNull( result.fromJs(Rp.result));
  const MdEval =sys.$checkNull( modelEval.fromJs(Rp["eval"]));
  const Nicks =sys.$checkNull( Rp.nicks); 
  const LastCloses =sys.$checkNull( Rp.lastCloses); 
  const Orders =sys.$checkNull( arr.map(Rp.orders, function(O)  {sys.$params(arguments.length, 1);  return order.fromJs(O);}));

  
   function mkTr(date, Buys, Sells, Portfolio, Quarantine, cash)  {sys.$params(arguments.length, 6);
    arr.sort(Portfolio, str.less);
    arr.sort(Quarantine, str.less);

     return Q("tr")
      .add(Q("td")
        .klass("lframe")
        .text(time.toIso(time.fromStr(date))))
      .add(Q("td")
        .klass("lframe")
        .style("width:100px")
        .text(arr.join(Buys, ", ")))
      .add(Q("td")
        .klass("lframe")
        .style("width:100px")
        .text(arr.join(Sells, ", ")))
      .add(Q("td")
        .klass("lframe")
        .style("width:300px")
        .text(arr.join(Portfolio, ", ") + " [" + arr.size(Portfolio) + "]"))
      .add(Q("td")
        .klass("lframe")
        .style("width:200px")
        .text(arr.join(Quarantine, ", ") + " [" + arr.size(Quarantine) + "]"))
      .add(Q("td")
        .klass("rframe")
        .text(math.toIso(cash, 2)))
    ;
  };

  const Assets =sys.$checkNull( [0]);
  const Trs =sys.$checkNull( []); 
  if (sys.asBool(arr.size(Orders) > 0)) {
    const LastDate =sys.$checkNull( [""]);
    const Cash =sys.$checkNull( [cts.initialCapital]);
    const Buys =sys.$checkNull( []); 
    const Sells =sys.$checkNull( []); 
    const Quarantine =sys.$checkNull( {}); 
    const Portfolio =sys.$checkNull( {}); 

    for (let O  of sys.$forObject( Orders)) {
      const date =sys.$checkNull( O.date);
      const QRemoves =sys.$checkNull( []); 
      for (let nk  of sys.$forObject( dic.keys(Quarantine)))
        if (sys.asBool(Quarantine[nk] <= date)) arr.push(QRemoves, nk);
      for (let nk  of sys.$forObject( QRemoves)) dic.remove(Quarantine, nk);

      if (sys.asBool(!sys.asBool(LastDate[0]))) {
        LastDate[0] =sys.$checkExists(LastDate[0],sys.$checkNull( date));
      } else if (sys.asBool(sys.$neq(date , LastDate[0]))) {
        arr.push(
          Trs,
          mkTr(
            LastDate[0], Buys, Sells,
            dic.keys(Portfolio), dic.keys(Quarantine), Cash[0]
          )
        );
        arr.clear(Buys);
        arr.clear(Sells);
        LastDate[0] =sys.$checkExists(LastDate[0],sys.$checkNull( date));
      }
      const nk =sys.$checkNull( O.nick);
      switch (O.type) {
        case order.sell:{ {
          arr.push(Sells, nk);
          const pr =sys.$checkNull( Portfolio[nk][1]);
          dic.remove(Portfolio, nk);
          Cash[0] +=sys.$checkExists(Cash[0],sys.$checkNull( broker.sell(O.stocks, O.price)));
          const dt0 =sys.$checkNull( time.fromStr(date));
          const dt =sys.$checkNull( time.addDays(dt0,sys.asBool( (O.price >= pr * cts.noLossMultiplicator))
            ? cts.daysWin
            : cts.daysLoss
          ));
          dic.put(Quarantine, nk, time.toStr(dt)); 
        }break;}
        case order.buy:{ {
          arr.push(Buys, nk);
          dic.put(Portfolio, nk, [O.stocks, O.price]);
          Cash[0] -=sys.$checkExists(Cash[0],sys.$checkNull( broker.buy(O.stocks, O.price)));
        }break;}
      }
    }
    arr.push(
      Trs,
      mkTr(
        LastDate[0], Buys, Sells,
        dic.keys(Portfolio), dic.keys(Quarantine), Cash[0]
      )
    );
    Assets[0] =sys.$checkExists(Assets[0],sys.$checkNull( arr.reduce(
      dic.toArr(Portfolio),
      Cash[0],
      function(r, Tp)  {sys.$params(arguments.length, 2);
         return r + broker.sell(
            Tp[1][0], LastCloses[arr.index(Nicks, function(n)  {sys.$params(arguments.length, 1);  return sys.$eq(n , Tp[0]);})]
          );}
    )));
  } else {
    arr.push(Trs, Q("tr")
      .add(Q("td")
        .att("rowspan", "5")
        .text(II("Without Data"))))
    ;
  }

  wg
    .removeAll()
    .add(Q("div")
      .klass("head")
      .text(II("Historic")))
    .add(Q("div").klass("separator"))
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("chead")
          .text(II("Model")))
        .adds(arr.map(Model.paramNames, function(n)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rhead")
            .text(n)
          ;})))
      .add(Q("tr")
        .add(Q("td")
          .klass("cframe")
          .text(Model.name))
        .adds(arr.fromIter(iter.map(
          iter.$range(0,arr.size(MdEval.params)),
          function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .text(fns.paramFmt(Model.paramTypes[i], MdEval.params[i]))
          ;})))))
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
          .text(math.toIso(fns.evaluate(Result.assets, Result.profits), 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Result.sales, 0)))))
    .add(Q("div").klass("separator2"))
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("H. Eval.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("H. Sales")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Eval.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Sales"))))
      .add(Q("tr")
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(fns.evaluate(MdEval.hassets, MdEval.hprofits), 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(MdEval.hsales, 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(fns.evaluate(MdEval.assets, MdEval.profits), 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(MdEval.sales, 0)))))
    .add(Q("div")
      .klass("head")
      .text(II("Assets")))
    .add(Q("table")
      .att("align", "center")
      .klass("white")
      .add(Q("tr")
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Assets[0], 2)))))
    .add(Q("div")
      .klass("head")
      .text(II("Orders")))
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("lhead")
          .style("text-align:center")
          .text(II("Date")))
        .add(Q("td")
          .klass("lhead")
          .style("text-align:center")
          .text(II("Buys")))
        .add(Q("td")
          .klass("lhead")
          .style("text-align:center")
          .text(II("Sells")))
        .add(Q("td")
          .klass("lhead")
          .style("text-align:center")
          .text(II("Portfolio")))
        .add(Q("td")
          .klass("lhead")
          .style("text-align:center")
          .text(II("Quarantine")))
        .add(Q("td")
          .klass("rhead")
          .style("text-align:center")
          .text(II("Cash"))))
      .adds(Trs))
  ;
};
