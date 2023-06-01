import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';







export  function paramFormatter(n1, n2)  {sys.$params(arguments.length, 2);
   function r4(n)  {sys.$params(arguments.length, 1);  return math.toIso(n, 4);};
  if (sys.asBool(sys.asBool(!sys.asBool(str.ends(r4(n1), "0"))) || sys.asBool(!sys.asBool(str.ends(r4(n2), "0")))))  return r4;
   function r3(n)  {sys.$params(arguments.length, 1);  return math.toIso(n, 3);};
  if (sys.asBool(sys.asBool(!sys.asBool(str.ends(r3(n1), "0"))) || sys.asBool(!sys.asBool(str.ends(r3(n2), "0")))))  return r3;
   function r2(n)  {sys.$params(arguments.length, 1);  return math.toIso(n, 2);};
  if (sys.asBool(sys.asBool(!sys.asBool(str.ends(r2(n1), "0"))) || sys.asBool(!sys.asBool(str.ends(r2(n2), "0")))))  return r2;
   function r1(n)  {sys.$params(arguments.length, 1);  return math.toIso(n, 1);};
  if (sys.asBool(sys.asBool(!sys.asBool(str.ends(r1(n1), "0"))) || sys.asBool(!sys.asBool(str.ends(r1(n2), "0")))))  return r1;
   return function(n)  {sys.$params(arguments.length, 1);  return math.toIso(n, 0);};
};



export  function valueColor(max, min)  {sys.$params(arguments.length, 2);
  const df =sys.$checkNull( max - min);
   return function(value)  {sys.$params(arguments.length, 1);
    const red =sys.$checkNull( math.toInt((max - value) * 256 / df ));
    const blue =sys.$checkNull( math.toInt((value - min) * 256 / df ));
     return "rgb(" + red + ",80," + blue + ")";
  };
};



export  function format00(n)  {sys.$params(arguments.length, 1);
  const r =sys.$checkNull( math.toStr(n));
  return sys.asBool( str.len(r) < 2) ? "0" + r : r;
};



export  function lastSunday()  {sys.$params(arguments.length, 0);
  const d =sys.$checkNull( time.now());
   return time.toStr(time.addDays(d,  -time.weekday(d)));
};
