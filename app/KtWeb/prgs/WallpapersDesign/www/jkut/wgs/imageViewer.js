import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);


export  function mk(Wg, url, width, onClick)  {sys.$params(arguments.length, 4);
  const height =sys.$checkNull( math.toInt(width * 9 / 16));
  const imgName =sys.$checkNull( sys.$slice(url,str.lastIndex(url, "/") + 1,null));
  const Img =sys.$checkNull( Q("img")
    .att("src", url + "?t=" + time.now())
    .att("title", imgName)
    .att("border", 1)
    .att("style", "cursor:pointer")
    .on("click", function(ev)  {sys.$params(arguments.length, 1); onClick();}))
  ;
  Img.on("load", function(ev)  {sys.$params(arguments.length, 1);
    const E =sys.$checkNull( Img.e);
    if (sys.asBool(E.width / E.height > 1.77777777)) Img.att("width", width);
    else Img.att("height", height);
  });
  Wg
    .removeAll()
    .add(Img)
  ;
};
