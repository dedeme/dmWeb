import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as cts from  "../data/cts.js";













export  function mk(id, cycle, model, params, assets, profits, points, sales)  {sys.$params(arguments.length, 8);
   return {id:id, cycle:cycle, model:model, params:params, assets:assets, profits:profits, points:points, sales:sales};};






export  function evaluate(assets, profits)  {sys.$params(arguments.length, 2);  return math.toInt(
    ( assets * cts.assetsRatio / cts.maxAssets +
        (1.0 + profits) * cts.profitsAvgRatio / cts.maxProfitsAvgRatio
      ) * 3000.0
  );};



export  function greater(F1, F2)  {sys.$params(arguments.length, 2); return sys.asBool(
  sys.$eq(F1.points , F2.points))
    ?sys.asBool( sys.$eq(F1.assets , F2.assets))
      ? F1.profits > F2.profits
      : F1.assets > F2.assets
    : F1.points > F2.points
  ;};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    A[0],
    A[1],
    A[2],
    A[3],
    A[4],
    A[5],
    evaluate(A[4], A[5]),
    A[6]
  );};
