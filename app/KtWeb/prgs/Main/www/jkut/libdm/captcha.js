import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);














export  function mk(storeId, counterLimit, zeroColor, oneColor)  {sys.$params(arguments.length, 4);
  const wg =sys.$checkNull( Q("div"));
  const now =sys.$checkNull( time.now());
  const Counter =sys.$checkNull( [getCounter(storeId)]);
  if (sys.asBool(now - getTime(storeId) > 900000)) {
    Counter[0] =sys.$checkExists(Counter[0],sys.$checkNull( 0));
    setCounter(storeId, 0);
    setTime(storeId, time.now());
  }
  const ch0 =sys.$checkNull( arr.fromIter(
    iter.map(iter.$range(0,4), function(i)  {sys.$params(arguments.length, 1);  return Q("input").att("type", "checkbox");})
  ));
  const ch1 =sys.$checkNull( arr.fromIter(
    iter.map(iter.$range(0,4), function(i)  {sys.$params(arguments.length, 1);  return Q("input").att("type", "checkbox");})
  ));

  

  
  
   function check()  {sys.$params(arguments.length, 0);  return sys.asBool(arr.all(ch0, function(ch)  {sys.$params(arguments.length, 1);  return !sys.asBool(ch.isChecked());})) &&
      sys.asBool(arr.all(ch1, function(ch)  {sys.$params(arguments.length, 1);  return ch.isChecked();}))
    ;};


  
  
   function increment()  {sys.$params(arguments.length, 0);
    setCounter(storeId, Counter[0] + 1);
    setTime(storeId, time.now());
  };

  
  
   function isUpLimit()  {sys.$params(arguments.length, 0);  return Counter[0] >= counterLimit;};

  
  
   function reset()  {sys.$params(arguments.length, 0);
    resetCounter(storeId);
    resetTime(storeId);
  };


  

  const tds =sys.$checkNull( arr.cat([
    arr.map(ch0, function(ch)  {sys.$params(arguments.length, 1);  return Q("td")
      .att("style", "border: 1px solid;background-color: " + zeroColor)
      .add(ch)
    ;}),
    arr.map(ch1, function(ch)  {sys.$params(arguments.length, 1);  return Q("td")
      .att("style", "border: 1px solid;background-color: " + oneColor)
      .add(ch)
    ;})
  ]));

  arr.shuffle(tds);
  const tds1 =sys.$checkNull( arr.take(tds, 4));
  const tds2 =sys.$checkNull( arr.drop(tds, 4));

  wg
    .removeAll()
    .add(Q("table")
      .att("border", 0)
      .style("border: 1px solid;background-color: #fffff0")
      .add(Q("tr")
        .adds(tds1))
      .add(Q("tr")
        .adds(tds2)))
  ;

   return {wg:wg, check:check, increment:increment, isUpLimit:isUpLimit, reset:reset};
};


 function getCounter(id)  {sys.$params(arguments.length, 1);
  const N =sys.$checkNull( storage.get(id + "_counter"));
  return sys.asBool( N) ? math.fromStr(N[0])[0] : 0;
};


 function setCounter(id, n)  {sys.$params(arguments.length, 2); storage.put(id + "_counter", math.toStr(n));};


 function resetCounter(id)  {sys.$params(arguments.length, 1); storage.del(id + "_counter");};


 function getTime(id)  {sys.$params(arguments.length, 1);
  const N =sys.$checkNull( storage.get(id + "_time"));
  return sys.asBool( N) ? math.fromStr(N[0])[0] : time.now();
};


 function setTime(id, n)  {sys.$params(arguments.length, 2); storage.put(id + "_time", math.toStr(n));};


 function resetTime(id)  {sys.$params(arguments.length, 1); storage.del(id + "_time");};
