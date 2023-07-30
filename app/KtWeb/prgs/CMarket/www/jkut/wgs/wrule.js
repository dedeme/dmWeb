import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);






export  function mk(width, color, title)  {sys.$params(arguments.length, 3);
   return Q("table")
    .klass("main")
    .style("color:" + color)
    .add(Q("tr")
      .add(Q("td")
        .style("width:" + width + "px;")
        .add(Q("hr")))
      .add(Q("td")
        .style("width:5px;white-space: nowrap;")
        .html(title))
      .add(Q("td")
        .add(Q("hr"))))
  ;
};




export  function mkBig(title)  {sys.$params(arguments.length, 1);  return mk(50, "#101010", title);};




export  function mkSmall(title)  {sys.$params(arguments.length, 1);  return mk(20, "#808080", title);};
