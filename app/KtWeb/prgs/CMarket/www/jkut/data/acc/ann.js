import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as opr from  "../../data/acc/opr.js";







export  function mk(id, date, op)  {sys.$params(arguments.length, 3);  return {id:id, date:date, op:op};};


export  function toJs(An)  {sys.$params(arguments.length, 1);
  const R =sys.$checkNull( [An.id, An.date]);
  arr.cat(R, opr.toJs(An.op));
   return R;
};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(A[0], A[1], opr.fromJs(sys.$slice(A,2,null)));};
