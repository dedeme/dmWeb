import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as log from  "../../libdm/log.js";
import * as cts from  "../../data/cts.js";
import * as paramsEval from  "../../data/paramsEval.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);



export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Main",
    source: "Home",
    rq: "idata"
  }));

  const iparsEvals =sys.$checkNull( arr.map(Rp.iparamsEvals, paramsEval.fromJs));
  const models =sys.$checkNull( Rp.models);
  const parsEvals =sys.$checkNull( arr.map(Rp.paramsEvals, paramsEval.fromJs));

  const logDiv =sys.$checkNull( Q("div"));

  
   async  function load(fn)  {sys.$params(arguments.length, 1);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      module: "Main",
      source: "Home",
      rq: "getLog"
    }));
    fn(arr.map(Rp.log, log.logRowFromJs));
  };

  
   async  function reset(fn)  {sys.$params(arguments.length, 1);
    await client.send({
      prg: cts.appName,
      module: "Main",
      source: "Home",
      rq: "resetLog"
    });
    fn();
  };

  
   function tlt(tx)  {sys.$params(arguments.length, 1);    
    return sys.$eq(tx,"All log entries will be deleted.\nContinue?")?
      II("All log entries will be deleted.\nContinue?"):
    sys.$eq(tx,"2 Days")? II("2 Days"):
    sys.$eq(tx,"All")? II("All"):
    sys.$eq(tx,"Reload")? II("Reload"):
    sys.$eq(tx,"Delete")? II("Delete"):
    sys.$eq(tx,"Errors")? II("Errors"):
    sys.$eq(tx,"Log")? II("Log"):
     tx
  ;};

  log.mk(logDiv, load, reset, tlt, true, 100, 25);

  wg
    .removeAll()
    .adds(arr.map([0, 1], function(i)  {sys.$params(arguments.length, 1);  return Q("div").klass("separator");}))
    .add(Q("table")
      .klass("frame")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(Q("span")
            .html(
              "Los mejores resultados de los modelos usados por los inversores son:<br>" +
              "<pre>" +
              arr.join(arr.fromIter(iter.map(iter.$range(0,models.length), function(i)  {sys.$params(arguments.length, 1);
                 return "Inv-" + i + " (" +
                sys.toStr(iparsEvals[i].Params) +
                ": " + math.toStr(iparsEvals[i].ev / 100) +
                ") -> " +
                models[i] + sys.toStr(parsEvals[i].Params) +
                ": " + math.toStr(parsEvals[i].ev / 100);})), "\n") +
              "</pre>"
            )))))
    .add(logDiv)
  ;
};
