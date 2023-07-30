import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as monthAnn from  "../data/monthAnn.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


const month =sys.$checkNull( 0);
const place =sys.$checkNull( 1);
const amount =sys.$checkNull( 2);



export  function mk(Ann, fclose, faccept)  {sys.$params(arguments.length, 3);
  const place0 =sys.$checkNull(sys.asBool( Ann[place]) ? Ann[place][0] : "");
  const entry =sys.$checkNull( Q("input")
    .att("type", "text")
    .att("id", "formEntry")
    .style("width:100px")
    .value(place0))
  ;
  entry.on("keydown", function(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(
      sys.asBool(sys.$eq(ev.code.toLowerCase() , "numpadenter")) ||
      sys.asBool(sys.$eq(ev.code.toLowerCase() , "enter")))
    ) {
      const pVal =sys.$checkNull( entry.getValue());
      if (sys.asBool(sys.$eq(pVal , place0))) {
        fclose();
      } else {
        const Place =sys.$checkNull(sys.asBool( sys.$eq(pVal , "")) ? [] : [pVal]);
        const A2 =sys.$checkNull( monthAnn.mk(
          Ann[month],
          Place,
          Ann[amount]
        ));
        faccept(A2);
      }
    } else if (sys.asBool(sys.$eq(ev.code.toLowerCase() , "escape"))) {
      fclose();
    }
  });

   return Q("table")
    .add(Q("tr")
      .add(Q("td")
        .klass("head")
        .text(II("Set Place"))))
    .add(Q("tr")
      .add(Q("td")
        .text(" ")))
    .add(Q("tr")
      .add(Q("td")
        .add(Q("span")
          .html(sys.$slice(Ann[month],4,null) + "/" + sys.$slice(Ann[month],null,4) + ":&nbsp;"))
        .add(entry)))
  ;
};
