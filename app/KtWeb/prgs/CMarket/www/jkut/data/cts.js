import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export const appName =sys.$checkNull( "CMarket");

export const version =sys.$checkNull( "202206");

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


export const meNick =sys.$checkNull( "<ME>");

export const ibexNick =sys.$checkNull( "<IBEX>");

export const euroNick =sys.$checkNull( "<EURO>");

export const usaNick =sys.$checkNull( "<USA>");


export const trading =sys.$checkNull( 1500000);

export const bet =sys.$checkNull( 10000.0);

export const initialCapital =sys.$checkNull( 100000.0);

export const minToBet =sys.$checkNull( 11000.0);

export const serverStopped =sys.$checkNull( 0);

export const serverActive =sys.$checkNull( 1);

export const serverSelected =sys.$checkNull( 2);

export const accountingQuotes =sys.$checkNull( 250);


export const okMsg =sys.$checkNull( II("Operation successfully done."));

export const failMsg =sys.$checkNull( II("Operation failed.\nSee log."));


export const toBuyColors =sys.$checkNull( [
  "rgba(160, 0, 0)",
  "rgba(224, 160, 0)",
  "rgba(240, 224, 0)"
]);

export const toSellColors =sys.$checkNull( [
  "rgba(0, 0, 160)",
  "rgba(0, 160, 224)",
  "rgba(0, 160, 0)"
]);


export const active =sys.$checkNull( "Active");


export const sleeping =sys.$checkNull( "Sleeping");


export const chartOrderNick =sys.$checkNull( 0);


export const chartOrderDay =sys.$checkNull( 1);


export const chartOrderSignal =sys.$checkNull( 2);
