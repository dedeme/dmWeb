import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);


export const appName =sys.$checkNull( "MMagazine");

export const version =sys.$checkNull( "202205");

export const foot =sys.$checkNull( Q("table")
  .klass("main")
  .add(Q("tr")
    .add(Q("td")
      .add(Q("hr"))))
  .add(Q("tr")
    .add(Q("td")
      .style("text-align: right;color:#808080;font-size:x-small;")
      .html(str.fmt("- © ºDeme. %v (%v) -", [appName, version])))))
;


export const ToBuyColors =sys.$checkNull( [
  "rgba(160, 0, 0)",
  "rgba(224, 160, 0)",
  "rgba(240, 224, 0)"
]);


export const ToSellColors =sys.$checkNull( [
  "rgba(0, 0, 160)",
  "rgba(0, 160, 224)",
  "rgba(0, 160, 0)"
]);
