import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as fns from  "../data/fns.js";

const II =sys.$checkNull( fns.tlt);





export  function mk(id, tm)  {sys.$params(arguments.length, 2);  return [id, tm];};


export  function timeToStr(A)  {sys.$params(arguments.length, 1);
  const now =sys.$checkNull( time.now());
   return (sys.asBool(sys.$eq(time.day(now) , time.day(A[1]))) ? II("Today") : II("Tomorrow")) +
    " " + II("at") + " " + sys.$slice(time.fmt("%t", A[1]),null, -3)
  ;
};



export  function compare(A1, A2)  {sys.$params(arguments.length, 2);
  const fmt =sys.$checkNull( "%Y%M%D%t");
  const d1 =sys.$checkNull( sys.$slice(time.fmt(fmt, fns.ftoi(A1[1])),null, -3));
  const d2 =sys.$checkNull( sys.$slice(time.fmt(fmt, fns.ftoi(A2[1])),null, -3));
  return sys.asBool( d1 > d2) ? 1:sys.asBool( d1 < d2) ?  -1 : 0;
};




export  function add(As, A)  {sys.$params(arguments.length, 2);
  if (sys.asBool(arr.any(As, function(A0)  {sys.$params(arguments.length, 1);  return sys.$eq(compare(A0, A) , 0);})))  return false;
  arr.push(As, A);
   return true;
};



export  function del(As, A)  {sys.$params(arguments.length, 2);
  arr.filterIn(As, function(A0)  {sys.$params(arguments.length, 1);  return sys.$neq(compare(A0, A) , 0);});
};



export  function update(As)  {sys.$params(arguments.length, 1);
  const tm =sys.$checkNull( time.now() - 180000); 
  arr.filterIn(As, function(A)  {sys.$params(arguments.length, 1);
     return fns.ftoi(A[1]) > tm;
  });
};
