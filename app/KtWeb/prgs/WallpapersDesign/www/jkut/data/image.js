import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as imgCut from  "../data/imgCut.js";
import * as imgAdjustment from  "../data/imgAdjustment.js";
import * as imgBlur from  "../data/imgBlur.js";



export  function mk(id, Cut, Adjustment, Blur)  {sys.$params(arguments.length, 4);  return {id:id, Cut:Cut, Adjustment:Adjustment, Blur:Blur};};


export  function setCut(I, V)  {sys.$params(arguments.length, 2);  return mk(I.id, V, I.Adjustment, I.Blur);};


export  function setAdjustment(I, V)  {sys.$params(arguments.length, 2);  return mk(I.id, I.Cut, V, I.Blur);};


export  function setBlur(I, V)  {sys.$params(arguments.length, 2);  return mk(I.id, I.Cut, I.Adjustment, V);};


export  function toJs(I)  {sys.$params(arguments.length, 1);  return [
    I.id,sys.asBool(
    I.Cut) ? [imgCut.toJs(I.Cut[0])] : [],sys.asBool(
    I.Adjustment) ? [imgAdjustment.toJs(I.Adjustment[0])] : [],sys.asBool(
    I.Blur) ? [imgBlur.toJs(I.Blur[0])] : []
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    A[0],sys.asBool(
    A[1]) ? [imgCut.fromJs(A[1][0])] : [],sys.asBool(
    A[2]) ? [imgAdjustment.fromJs(A[2][0])] : [],sys.asBool(
    A[3]) ? [imgBlur.fromJs(A[3][0])] : []
  );};
