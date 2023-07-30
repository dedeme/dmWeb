import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as model from  "../../data/model.js";
import * as istrategy from  "../../data/investor/istrategy.js";





export  function mk(base, nicks)  {sys.$params(arguments.length, 2);  return {base:base, nicks:nicks};};


export  function toJs(I)  {sys.$params(arguments.length, 1);  return [
    istrategy.toJs(I.base),
    arr.map(dic.toArr(I.nicks), function(Tp)  {sys.$params(arguments.length, 1);  return [Tp[0], istrategy.toJs(Tp[1])];})
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    istrategy.fromJs(A[0]),
    dic.fromArr(arr.map(A[1], function(Tp)  {sys.$params(arguments.length, 1);  return [Tp[0], istrategy.fromJs(Tp[1])];}))
  );};
