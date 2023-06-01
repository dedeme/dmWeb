import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as log from  "../libdm/log.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(wg)  {sys.$params(arguments.length, 1);
  const logDiv =sys.$checkNull( Q("div"));

  
   async  function load(fn)  {sys.$params(arguments.length, 1);
    const Rp =sys.$checkNull( await  client.send({
      prg: "Main",
      source: "Home",
      rq: "getLog"
    }));
    fn(arr.map(Rp.log, log.logRowFromJs));
  };

  
   async  function reset(fn)  {sys.$params(arguments.length, 1);
    await client.send({
      prg: "Main",
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
    .add(logDiv)
  ;
};
