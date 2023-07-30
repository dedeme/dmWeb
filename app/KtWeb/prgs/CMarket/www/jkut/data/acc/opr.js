import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';





export const seT =sys.$checkNull( "se");

export const buT =sys.$checkNull( "bu");

export const stT =sys.$checkNull( "st");

export const inT =sys.$checkNull( "in");

export const wiT =sys.$checkNull( "wi");

export const prT =sys.$checkNull( "pr");

export const feT =sys.$checkNull( "fe");

export const pdT =sys.$checkNull( "pd");

export const ndT =sys.$checkNull( "nd");


 function mk(tp, i, f, s)  {sys.$params(arguments.length, 4);  return {tp:tp, i:i, f:f, s:s};};



export  function mkSe(nick, stocks, price)  {sys.$params(arguments.length, 3);  return mk(seT, stocks, price, nick);};



export  function mkBu(nick, stocks, price)  {sys.$params(arguments.length, 3);  return mk(buT, stocks, price, nick);};



export  function mkSt(nick, stocks, price)  {sys.$params(arguments.length, 3);  return mk(stT, stocks, price, nick);};



export  function mkIn(amount)  {sys.$params(arguments.length, 1);  return mk(inT, 0, amount, "");};



export  function mkWi(amount)  {sys.$params(arguments.length, 1);  return mk(wiT, 0, amount, "");};



export  function mkPr(amount, cause)  {sys.$params(arguments.length, 2);  return mk(prT, 0, amount, cause);};



export  function mkFe(amount, cause)  {sys.$params(arguments.length, 2);  return mk(feT, 0, amount, cause);};



export  function mkPd(amount, cause)  {sys.$params(arguments.length, 2);  return mk(pdT, 0, amount, cause);};


export  function mkNd(amount, cause)  {sys.$params(arguments.length, 2);  return mk(ndT, 0, amount, cause);};



export  function type(op)  {sys.$params(arguments.length, 1);  return op.tp;};



export  function nick(op)  {sys.$params(arguments.length, 1);  return op.s;};



export  function stocks(op)  {sys.$params(arguments.length, 1);  return op.i;};



export  function price(op)  {sys.$params(arguments.length, 1);  return op.f;};



export  function amount(op)  {sys.$params(arguments.length, 1);  return op.f;};



export  function cause(op)  {sys.$params(arguments.length, 1);  return op.s;};


export  function toJs(op)  {sys.$params(arguments.length, 1); switch(op.tp) {
    case seT: case buT: case stT:{  return [op.tp, op.s, op.i, op.f];break;}
    case prT: case feT: case pdT: case ndT:{  return [op.tp, op.f, op.s];break;}
    case inT: case wiT:{  return [op.tp, op.f];break;}
    default:{ throw new Error(("Unknown operation of type " + op.tp));}
  }};



export  function fromJs(A)  {sys.$params(arguments.length, 1);
  const tp =sys.$checkNull( A[0]);
  switch(tp) {
    case seT: case buT: case stT:{  return mk(tp, A[2], A[3], A[1]);break;}
    case prT: case feT: case pdT: case ndT:{  return mk(tp, 0, A[1], A[2]);break;}
    case inT: case wiT:{  return mk(tp, 0, A[1], "");break;}
    default:{ throw new Error(("Unknown operation of type " + tp));}
  }
};
