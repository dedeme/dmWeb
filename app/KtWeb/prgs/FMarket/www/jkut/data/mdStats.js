import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as flea from  "../data/flea.js";





export function mkGroup (ModelIds, duplicates)  {sys.$params(arguments.length, 2);  return {ModelIds:ModelIds, duplicates:duplicates};};


export function groupFormatIds (G)  {sys.$params(arguments.length, 1);  return arr.join(arr.map(G.ModelIds, function(id)  {sys.$params(arguments.length, 1);
    const Id = sys.$checkNull([id]);
    while (sys.asBool(str.len(Id[0]) < 5)) Id[0] +=sys.$checkExists(Id[0], sys.$checkNull(" "));
     return Id[0];
  }), " | ");};


export function groupFromJs (A)  {sys.$params(arguments.length, 1);  return mkGroup(A[0], A[1]);};



export function mkOrderFlea (order, Flea)  {sys.$params(arguments.length, 2);  return {order:order, Flea:Flea};};


export function orderFleaFromJs (A)  {sys.$params(arguments.length, 1);  return mkOrderFlea(A[0], flea.fromJs(A[1]));};



export function mkModel (modelId, Bests, Worsts, nfleas, position, assets)  {sys.$params(arguments.length, 6);  return {
    modelId:modelId, Bests:Bests, Worsts:Worsts, nfleas:nfleas, position:position, assets:assets
  };};


export function modelFromJs (A)  {sys.$params(arguments.length, 1);  return mkModel(
    A[0],
    arr.map(A[1], orderFleaFromJs),
    arr.map(A[2], orderFleaFromJs),
    A[3],
    A[4],
    A[5]
  );};





export function mk (Models, GroupsRanking)  {sys.$params(arguments.length, 2);  return {Models:Models, GroupsRanking:GroupsRanking};};


export function fromJs (A)  {sys.$params(arguments.length, 1);  return mk(
    arr.map(A[0], modelFromJs),
    arr.map(A[1], groupFromJs)
  );};
