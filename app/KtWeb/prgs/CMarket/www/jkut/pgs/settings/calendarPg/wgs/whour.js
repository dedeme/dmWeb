import * as iter from '../../../../_js/iter.js';import * as str from '../../../../_js/str.js';import * as bytes from '../../../../_js/bytes.js';import * as cryp from '../../../../_js/cryp.js';import * as dic from '../../../../_js/dic.js';import * as timer from '../../../../_js/timer.js';import * as js from '../../../../_js/js.js';import * as storage from '../../../../_js/storage.js';import * as sys from '../../../../_js/sys.js';import * as math from '../../../../_js/math.js';import * as domo from '../../../../_js/domo.js';import * as ui from '../../../../_js/ui.js';import * as arr from '../../../../_js/arr.js';import * as time from '../../../../_js/time.js';import * as client from '../../../../_js/client.js';import * as b64 from '../../../../_js/b64.js';




import * as fns from  "../../../../data/fns.js";

const Q =sys.$checkNull( ui.q);






export  function mk(hour, minute, fn)  {sys.$params(arguments.length, 3);
  const Hlist =sys.$checkNull( arr.fromIter(iter.map(iter.$range(0,24), function(n)  {sys.$params(arguments.length, 1);
     return (sys.asBool(sys.$eq(n , hour)) ? "+" : "") + fns.format00(n);}
  )));

  const Mlist =sys.$checkNull( arr.fromIter(iter.map(iter.$range(0,12), function(n)  {sys.$params(arguments.length, 1);
     return (sys.asBool(sys.$eq(n * 5 , minute)) ? "+" : "") + fns.format00(n * 5);}
  )));

  const h =sys.$checkNull( ui.select("wh", Hlist).on("change", function(e)  {sys.$params(arguments.length, 1); fn();}));
  const m =sys.$checkNull( ui.select("wm", Mlist).on("change", function(e)  {sys.$params(arguments.length, 1); fn();}));

  const wg =sys.$checkNull( Q("table")
    .add(Q("tr")
      .add(Q("td")
        .add(h))
      .add(Q("td")
        .add(Q("span")
          .html("<big> : </big>")))
      .add(Q("td")
        .add(m))))
  ;
   return {wg:wg, h:h, m:m};
};




export  function wg(w)  {sys.$params(arguments.length, 1);  return w.wg;};



export  function hour(w)  {sys.$params(arguments.length, 1);  return w.h.e.selectedIndex;};



export  function minute(w)  {sys.$params(arguments.length, 1);  return w.m.e.selectedIndex * 5;};
