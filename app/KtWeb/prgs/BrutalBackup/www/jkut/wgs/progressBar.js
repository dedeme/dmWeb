import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);










export  function mk(wg, fend)  {sys.$params(arguments.length, 2);
  const absoluteSpan =sys.$checkNull( Q("span"));
  const relativeSpan =sys.$checkNull( Q("span"));
  const progressTable =sys.$checkNull( Q("table"));
  const end =sys.$checkNull( math.toInt(fend));

  
  
   function show()  {sys.$params(arguments.length, 0);
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(absoluteSpan)))
        .add(Q("tr")
          .add(Q("td")
            .add(progressTable)))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(relativeSpan))))
    ;
  };

  
  
   function lock(msg)  {sys.$params(arguments.length, 1);
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .text(msg))))
    ;
  };

  
  
   function setValue(value)  {sys.$params(arguments.length, 1);
    const width =sys.$checkNull( 400);
    const val =sys.$checkNull(sys.asBool( (value > end)) ? end : math.toInt(value));

    absoluteSpan
      .removeAll()
      .text("" + math.toIso(val, 0) + " / " + math.toIso(end, 0))
    ;

    relativeSpan
      .removeAll()
      .text(sys.asBool(end > 0) ? (math.toIso(val * 100 / end, 2) + "%") : "");

    const tds =sys.$checkNull( [
      Q("td")
        .klass("border")
        .style(
            "height:5px;background:#000080;width:" +
            math.toInt(val * width / end) + "px"
          )
    ]);
    if (sys.asBool(sys.$neq(end , val))) tds.push(Q("td"));

    progressTable
      .removeAll()
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .add(Q("table")
            .style("border-collapse : collapse;width:" + width + "px")
            .add(Q("tr")
              .adds(tds)))))
    ;
  };

   return {show:show, lock:lock, setValue:setValue};
};
