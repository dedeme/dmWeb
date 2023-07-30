import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as model from  "../data/model.js";
import * as modelEval from  "../data/modelEval.js";
import * as i18n from  "../i18n.js";
import * as fns from  "../fns.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 async  function mk2(wg, modelId, historicOrder)  {sys.$params(arguments.length, 3);
  const Rp =sys.$checkNull( await  client.send({
    prg: "CMMarket",
    source: "ResultsPg",
    rq: "idata",
    modelId:modelId
  }));
  const Md =sys.$checkNull( model.fromJs(Rp.model));
  const Evals =sys.$checkNull( arr.map(Rp.evals, modelEval.fromJs));
  if (sys.asBool(historicOrder))
    arr.sort(Evals, function(E1, E2)  {sys.$params(arguments.length, 2);
       return fns.evaluate(E2.hassets, E2.hprofits) <
        fns.evaluate(E1.hassets, E1.hprofits);}
    );
  else
    arr.sort(Evals, function(E1, E2)  {sys.$params(arguments.length, 2);
       return fns.evaluate(E2.assets, E2.profits) <
        fns.evaluate(E1.assets, E1.profits);}
    );

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .adds(arr.fromIter(iter.map(
            iter.$range(0,1+arr.size(Evals[0].params)),
            function(i)  {sys.$params(arguments.length, 1);  return Q("td");}
          )))
        .add(Q("td")
          .klass("chead"))
        .add(Q("td")
          .klass("chead")
          .att("colspan", 4)
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); mk2(wg, modelId, false);})
            .klass("link")
            .text(II("Current"))))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("chead")
          .att("colspan", 4)
          .add(ui.link(function(e)  {sys.$params(arguments.length, 1); mk2(wg, modelId, true);})
            .klass("link")
            .text(II("Historic")))))
      .add(Q("tr")
        .add(Q("td"))
        .adds(arr.map(Md.paramNames, function(n)  {sys.$params(arguments.length, 1);  return Q("td")
          .klass("rhead")
          .text(n);}))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rhead")
          .text(II("Assets")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Prfs.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Eval.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Sls.")))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rhead")
          .text(II("Assets")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Prfs.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Eval.")))
        .add(Q("td")
          .klass("rhead")
          .text(II("Sls."))))
      .adds(arr.map(Evals, function(Ev)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .add(Q("a")
            .klass("link")
            .att(
                "href",
                "?" + Md.id +
                "&charts&" +
                js.w(Ev.params)
              )
            .add(ui.img("see"))))
        .adds(arr.fromIter(iter.map(iter.$range(0,arr.size(Ev.params)), function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .text(fns.paramFmt(Md.paramTypes[i], Ev.params[i]))
          ;})))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.assets, 2)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.profits * 100, 2) + "%"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(fns.evaluate(Ev.assets, Ev.profits), 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.sales, 0)))
        .add(Q("td").klass("rhead"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.hassets, 2)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.hprofits * 100, 2) + "%"))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(fns.evaluate(Ev.hassets, Ev.hprofits), 0)))
        .add(Q("td")
          .klass("rframe")
          .text(math.toIso(Ev.hsales, 0)));}))
      )
  ;

};


export  function mk(wg, modelId)  {sys.$params(arguments.length, 2); mk2(wg, modelId, false);};
