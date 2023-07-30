import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';




import * as i18n from  "./i18n.js";

const Q =sys.$checkNull( ui.q);



export  function mkTh(label, fn)  {sys.$params(arguments.length, 2);  return Q("td")
    .klass("header")
    .add(ui.link(function(e)  {sys.$params(arguments.length, 1); fn();})
      .klass("link")
      .text(label))
  ;};



export  function mkTdN(n, dec)  {sys.$params(arguments.length, 2);  return Q("td")
    .klass("fnumber")
    .text(sys.asBool(sys.$eq(i18n.getLang() , "es"))
        ? math.toIso(n, dec)
        : math.toEn(n, dec)
      )
  ;};
