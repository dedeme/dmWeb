import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';










export  function mkEntry(name, doc, code, link)  {sys.$params(arguments.length, 4);
   return { name:name, doc:doc, code:code, link:link };};


export  function entryToJs(D)  {sys.$params(arguments.length, 1);  return [D.name, D.doc, D.code, D.link];};


export  function entryFromJs(A)  {sys.$params(arguments.length, 1);  return mkEntry(A[0], A[1], A[2], A[3]);};






export  function mk(doc, Functions, Values)  {sys.$params(arguments.length, 3);  return { doc:doc, Functions:Functions, Values:Values };};


export  function toJs(D)  {sys.$params(arguments.length, 1);  return [
    D.doc,
    arr.map(D.Functions, entryToJs),
    arr.map(D.Values, entryToJs)
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    A[0],
    arr.map(A[1], entryFromJs),
    arr.map(A[2], entryFromJs)
  );};
