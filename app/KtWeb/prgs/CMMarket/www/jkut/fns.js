import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';




import * as model from  "./data/model.js";
import * as cts from  "./data/cts.js";






export  function evaluate(assets, profits)  {sys.$params(arguments.length, 2);  return math.toInt(
    ( assets * cts.assetsRatio / cts.maxAssets +
        (1 + profits) * cts.profitsAvgRatio / cts.maxProfitsAvgRatio
      ) * 3000
  );};




export  function paramFmt(type, number)  {sys.$params(arguments.length, 2);   
    return sys.$eq(type,model.percParam)? sys.$slice(math.toIso(number, 4),2,null):
     math.toIso(number, 0)
  ;};
