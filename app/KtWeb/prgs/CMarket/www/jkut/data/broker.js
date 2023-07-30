import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';







export  function buyFees(amount)  {sys.$params(arguments.length, 1); 
    return (sys.asBool( amount > 50000.0)       
        ? amount * 0.001
        : 9.75
    ) +
    amount * 0.00003 +       
    0.11 +                   
    amount * 0.002           
  ;};





export  function buy(stocks, price)  {sys.$params(arguments.length, 2);
  const amount =sys.$checkNull( stocks * price);
   return amount + buyFees(amount);
};




export  function sellFees(amount)  {sys.$params(arguments.length, 1); 
    return (sys.asBool( amount > 50000.0)       
        ? amount * 0.001
        : 9.75
    ) +
    amount * 0.00003 +       
    0.11                    
  ;};





export  function sell(stocks, price)  {sys.$params(arguments.length, 2);
  const amount =sys.$checkNull( stocks * price);
   return amount - sellFees(amount);
};
