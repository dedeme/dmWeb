import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';





export const typePERIODIC =sys.$checkNull( 0);

export const typeFIX =sys.$checkNull( 1);

export const typeMANUAL =sys.$checkNull( 2);











export  function mk(id, type, data, text)  {sys.$params(arguments.length, 4);  return {id:id, type:type, data:data, text:text};};



export  function days(Ann)  {sys.$params(arguments.length, 1);
  if (sys.asBool(sys.$neq(Ann.type , typePERIODIC)))
    throw new Error(("Expected type typePERIODIC, but it is " + Ann.data));
   return js.r(Ann.data)[1];
};



export  function date(Ann)  {sys.$params(arguments.length, 1);
  if (sys.asBool(sys.$eq(Ann.type , typeMANUAL)))
    throw new Error(("type must not be Ann_MANUAL"));
  if (sys.asBool(sys.$eq(Ann.type , typeFIX)))
     return js.r(Ann.data) * 1000;
   return js.r(Ann.data)[0] * 1000;
};


export  function toJs(A)  {sys.$params(arguments.length, 1);  return [
    A.id,
    A.type,
    js.r(A.data),
    A.text
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);
   return mk (
    A[0],
    A[1],
    js.w(A[2]),
    A[3]
  );
};
