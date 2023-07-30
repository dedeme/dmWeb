import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as bar from  "../wgs/bar.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


const date =sys.$checkNull( 0);
const description =sys.$checkNull( 1);
const amount =sys.$checkNull( 2);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Stays",
    rq: "idata"
  }));
  const Anns =sys.$checkNull( Rp.anns);

  
   function table()  {sys.$params(arguments.length, 0);
    const max =sys.$checkNull( arr.reduce(
      Anns, 0.0, function(r, An)  {sys.$params(arguments.length, 2); return sys.asBool( An[amount] > r) ? An[amount] : r;}
    ));
    const tb =sys.$checkNull( Q("table").att("align", "center").klass("border"));
     function barWg(A)  {sys.$params(arguments.length, 1);  return bar.mkWg(
        bar.mk(300, 2, A[amount] / max, "#c080c0", "#ffffff")
      );};

    return sys.asBool( Anns)
      ? tb
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(II("Date")))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(II("Place")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(II("Amount")))
          .add(Q("td").klass("header")))
        .adds(arr.map(Anns, function(A)  {sys.$params(arguments.length, 1);  return Q("tr")
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
              .text(
                  sys.$slice(A[date],6,null) + "/" +
                  sys.$slice(A[date],4,6) + "/" +
                  sys.$slice(A[date],null,4))
                )
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
              .html(A[description] + "&nbsp;"))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(fns.cFmt(A[amount])))
            .add(Q("td")
              .add(barWg(A)));}))
      : tb
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: center")
            .html(II("Without Data"))))
    ;
  };

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(table()))))
  ;
};
