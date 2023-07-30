import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';






export  function mk(date, open, close, max, min, vol, error)  {sys.$params(arguments.length, 7);
   return {date:date, open:open, close:close, max:max, min:min, vol:vol, error:error};};


export  function toStr(Qt)  {sys.$params(arguments.length, 1);  return str.fmt(
    "%v:%v:%v:%v:%v:%v:%v",
    [Qt.date, Qt.open, Qt.close, Qt.max, Qt.min, Qt.vol, Qt.error]
  );};


export  function fromStr(s)  {sys.$params(arguments.length, 1);
  const ps =sys.$checkNull( str.split(s, ":"));
   return mk(
    ps[0],
    math.fromStr(ps[1]),
    math.fromStr(ps[2]),
    math.fromStr(ps[3]),
    math.fromStr(ps[4]),
    math.fromStr(ps[5]),
    sys.$eq(ps[6] , "true")
  );
};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);};
