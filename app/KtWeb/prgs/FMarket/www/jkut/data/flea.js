import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as fmodel from  "../data/fmodel.js";
import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";








export function mk (id, cycle, isMale, Models, assets)  {sys.$params(arguments.length, 5);  return {
    id:id, cycle:cycle, isMale:isMale, Models:Models, assets:assets
  };};


export function fmtId (F)  {sys.$params(arguments.length, 1);
  const R = sys.$checkNull(["" + F.id]);
  while (sys.asBool(str.len(R[0]) < 9)) R[0] =sys.$checkExists(R[0], sys.$checkNull("0" + R[0]));
   return R[0];
};


export function fmtCycle (F)  {sys.$params(arguments.length, 1);
  const R = sys.$checkNull(["" + F.cycle]);
  while (sys.asBool(str.len(R[0]) < 4)) R[0] =sys.$checkExists(R[0], sys.$checkNull("0" + R[0]));
   return R[0];
};


export function fmtModels (F)  {sys.$params(arguments.length, 1);
  
  function fmt (Md)  {sys.$params(arguments.length, 1);
    const R = sys.$checkNull([Md.modelId]);
    while (sys.asBool(str.len(R[0]) < 5)) R[0] +=sys.$checkExists(R[0], sys.$checkNull(" "));
     return R[0] + "[" +
      arr.join(arr.map(Md.Params, function(n)  {sys.$params(arguments.length, 1);  return fns.nFormat2(n, 4);}), ", ") +
      "]"
    ;
  };
   return arr.map(F.Models, fmt);
};


export function fmtModels2 (F)  {sys.$params(arguments.length, 1);  return arr.join(arr.map(
    F.Models, function(M)  {sys.$params(arguments.length, 1);  return "" + cts.modelIxs[M.modelId];}
  ), "");};


export function fromJs (A)  {sys.$params(arguments.length, 1);  return mk (
    A[0], A[1], A[2], arr.map(A[3], fmodel.fromJs), A[4]
  );};
