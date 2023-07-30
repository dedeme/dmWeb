import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as model from  "../data/model.js";
import * as i18n from  "../i18n.js";
import * as fns from  "../fns.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg, modelId)  {sys.$params(arguments.length, 2);
  const Rp =sys.$checkNull( await  client.send({
    prg: "CMMarket",
    source: "Description",
    rq: "idata",
    modelId:modelId
  }));
  const Md =sys.$checkNull( model.fromJs(Rp.model));

  wg
    .removeAll()
    .add(Q("div")
      .klass("head")
      .text(Md.name))
    .add(Q("div").klass("separator"))
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Id") + ":"))
        .add(Q("td")
          .klass("rframe")
          .text(Md.id)))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Name") + ":"))
        .add(Q("td")
          .klass("rframe")
          .text(Md.name))))
    .add(Q("div").klass("separator"))
    .add(Q("table")
      .att("align", "center")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td"))
        .adds(arr.map(Md.paramNames, function(n)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rhead")
            .text(n)
          ;})))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Base") + ":"))
        .adds(arr.fromIter(iter.map(
          iter.$range(0,arr.size(Md.paramNames)),
          function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .text(fns.paramFmt(Md.paramTypes[i], Md.paramBases[i]))
          ;}))))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Base Increment") + ":"))
        .adds(arr.fromIter(iter.map(
          iter.$range(0,arr.size(Md.paramNames)),
          function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .text(fns.paramFmt(Md.paramTypes[i], Md.paramBaseIncs[i]))
          ;}))))
      .add(Q("tr")
        .add(Q("td")
          .klass("rhead")
          .text(II("Environment Increment") + ":"))
        .adds(arr.fromIter(iter.map(
          iter.$range(0,arr.size(Md.paramNames)),
          function(i)  {sys.$params(arguments.length, 1);  return Q("td")
            .klass("rframe")
            .text(fns.paramFmt(Md.paramTypes[i], Md.paramEnvIncs[i]))
          ;})))))
    .add(Q("div").klass("separator"))
    .add(Q("div")
      .klass("frame")
      .html(Md.doc))
  ;

};
