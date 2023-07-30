import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as model from  "../data/model.js";
import * as modelEval from  "../data/modelEval.js";
import * as i18n from  "../i18n.js";
import * as fns from  "../fns.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 function color(max, min, value)  {sys.$params(arguments.length, 3);
  const df =sys.$checkNull( max - min);
  const red =sys.$checkNull( math.toInt((max - value) * 256 / df ));
  const blue =sys.$checkNull( math.toInt((value - min) * 256 / df ));
   return "rgb(" + red + ",80," + blue + ")";
};



 async  function mk2(wg, modelId, isHistoric)  {sys.$params(arguments.length, 3);
  const Rp =sys.$checkNull( await  client.send({
    prg: "CMMarket",
    source: "ResultsPg",
    rq: "idata",
    modelId:modelId
  }));
  const Md =sys.$checkNull( model.fromJs(Rp.model));
  const Evals =sys.$checkNull( arr.map(Rp.evals, modelEval.fromJs));

  const Max =sys.$checkNull(sys.asBool( isHistoric)
    ? [fns.evaluate(Evals[0].hassets, Evals[0].hprofits)]
    : [fns.evaluate(Evals[0].assets, Evals[0].profits)])
  ;
  const Min =sys.$checkNull( [Max[0]]);
  for (let Eval  of sys.$forObject( Evals)) {
    const ev =sys.$checkNull(sys.asBool( isHistoric)
      ? fns.evaluate(Eval.hassets, Eval.hprofits)
      : fns.evaluate(Eval.assets, Eval.profits))
    ;
    if (sys.asBool(ev > Max[0])) Max[0] =sys.$checkExists(Max[0],sys.$checkNull( ev));
    else if (sys.asBool(ev < Min[0])) Min[0] =sys.$checkExists(Min[0],sys.$checkNull( ev));
  }
  const max =sys.$checkNull( Max[0]);
  const min =sys.$checkNull( Min[0]);

  
   function oneParam()  {sys.$params(arguments.length, 0);
     return Q("table")
      .klass("flat")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(Md.paramNames[0]))
        .add(Q("td")))
      .adds(arr.map(Evals, function(Eval)  {sys.$params(arguments.length, 1);
        const ev =sys.$checkNull(sys.asBool( isHistoric)
          ? fns.evaluate(Eval.hassets, Eval.hprofits)
          : fns.evaluate(Eval.assets, Eval.profits))
        ;
         return Q("tr")
          .add(Q("td")
            .klass("rframe")
            .style("font-size:small")
            .text(fns.paramFmt(Md.paramTypes[0], Eval.params[0])))
          .add(Q("td")
            .klass("border")
            .att("title", math.toIso(ev, 0))
            .style(
              "width:50px;" +
              "cursor:pointer;" +
              "background:" + color(max, min, ev)
            ))
            .on("click", function(e)  {sys.$params(arguments.length, 1); window.location.assign(
              "?" + Md.id + "&charts&" + js.w([Eval.params[0]])
            );})
      ;}))
    ;
  };

  
   function twoParams()  {sys.$params(arguments.length, 0);
    const pm1 =sys.$checkNull( Evals[0].params[0]);
    const ncols =sys.$checkNull( iter.count(iter.takeWhile(
      arr.toIter(Evals),
      function(Eval)  {sys.$params(arguments.length, 1);  return sys.$eq(Eval.params[0] , pm1);}
    )));
    const Row =sys.$checkNull( [0.0]);
    for (let j = 0;j < ncols; ++j) arr.push(Row, Evals[j].params[1]);
    const Rows =sys.$checkNull( [Row]);
    for (let i = 0;i < math.toInt(arr.size(Evals) / ncols); ++i) {
      const Row =sys.$checkNull( [Evals[i * ncols].params[0]]);
      for (let j = 0;j < ncols; ++j) {
        const Eval =sys.$checkNull( Evals[i * ncols + j]);
        const ev =sys.$checkNull(sys.asBool( isHistoric)
          ? fns.evaluate(Eval.hassets, Eval.hprofits)
          : fns.evaluate(Eval.assets, Eval.profits))
        ;
        arr.push(Row, ev);
      }
      arr.push(Rows, Row);
    }

     return Q("table")
      .klass("flat")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .klass("rhead")
          .text(Md.paramNames[0]))
        .adds(arr.fromIter(iter.map(iter.$range(0,ncols), function(i)  {sys.$params(arguments.length, 1);  return Q("td");}))))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(Md.paramNames[1]))
        .add(Q("td"))
        .adds(arr.fromIter(iter.map(iter.$range(1,ncols), function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .style("font-size:small")
            .text(fns.paramFmt(Md.paramTypes[1], Rows[0][i]))
          ;}))))
      .adds(arr.fromIter(iter.map(iter.$range(1, arr.size(Rows)), function(i)  {sys.$params(arguments.length, 1);  return Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .klass("rframe")
            .style("font-size:small")
            .text(fns.paramFmt(Md.paramTypes[0], Rows[i][0])))
          .adds(arr.fromIter(iter.map(iter.$range(1,ncols), function(j)  {sys.$params(arguments.length, 1);  return Q("td")
              .klass("border")
              .att("title", math.toIso(Rows[i][j], 0))
              .style(
                  "cursor:pointer;" +
                  "background:" + color(max, min, Rows[i][j])
                )
              .on("click", function(e)  {sys.$params(arguments.length, 1); window.location.assign(
                  "?" + Md.id + "&charts&" + js.w([Rows[i][0], Rows[0][j]])
                );})
            ;})))
          ;})))
    ;
  };

  const Lopts =sys.$checkNull( [
    menu.toption("c", II("Current"), function()  {sys.$params(arguments.length, 0); mk2(wg, modelId, false);}),
    menu.separator(),
    menu.toption("h", II("Historic"), function()  {sys.$params(arguments.length, 0); mk2(wg, modelId, true);})
  ]);
  const menuWg =sys.$checkNull( menu.mk(Lopts, [],sys.asBool( isHistoric) ? "h" : "c", false));

  wg
    .removeAll()
    .add(menuWg)
    .add(sys.asBool(sys.$eq(arr.size(Md.paramNames) , 1)) ? oneParam() : twoParams())
  ;

};


export  function mk(wg, modelId)  {sys.$params(arguments.length, 2); mk2(wg, modelId, false);};
