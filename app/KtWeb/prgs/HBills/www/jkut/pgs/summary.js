import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';






import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as bar from  "../wgs/bar.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


const date =sys.$checkNull( 0);

const month =sys.$checkNull( 0);

const amount =sys.$checkNull( 2);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Summary",
    rq: "idata"
  }));
  const Years =sys.$checkNull( Rp.years);
  const BillAnns =sys.$checkNull( Rp.billAnns);
  const StayAnns =sys.$checkNull( Rp.stayAnns);

  
   function table()  {sys.$params(arguments.length, 0);
    
     function fmax(n1, n2)  {sys.$params(arguments.length, 2); return sys.asBool( n1 > n2) ? n1 : n2;};
    
    const Rows =sys.$checkNull( arr.map(Years, function(y)  {sys.$params(arguments.length, 1);  return [
        y,
        arr.reduce(
          arr.filter(BillAnns, function(A)  {sys.$params(arguments.length, 1);  return str.starts(A[month], y);}),
          0.0, function(r, A)  {sys.$params(arguments.length, 2);  return r + A[amount];}
        ),
        arr.reduce(
          arr.filter(StayAnns, function(A)  {sys.$params(arguments.length, 1);  return str.starts(A[date], y);}),
          0.0, function(r, A)  {sys.$params(arguments.length, 2);  return r + A[amount];}
        )
      ];}));
    const max =sys.$checkNull( arr.reduce(Rows, 0.0, function(r, Row)  {sys.$params(arguments.length, 2);  return fmax(Row[1] + Row[2], r);}));
    const sumBills =sys.$checkNull( arr.reduce(Rows, 0.0, function(r, Row)  {sys.$params(arguments.length, 2);  return r + Row[1];}));
    const sumStays =sys.$checkNull( arr.reduce(Rows, 0.0, function(r, Row)  {sys.$params(arguments.length, 2);  return r + Row[2];}));
     function barWg(Row)  {sys.$params(arguments.length, 1);  return bar.mkWg(
        bar.mk(300, 2, (Row[1] + Row[2]) / max, "#c0c080", "#ffffff")
      );};

    
      return Q("table")
        .att("align", "center")
        .klass("border")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .html(II("Year")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(II("Bills")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(II("Stays")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .html(II("Total")))
          .add(Q("td").klass("header")))
        .adds(arr.map(Rows, function(R)  {sys.$params(arguments.length, 1);  return Q("tr")
            .add(Q("td")
              .klass("border")
              .style("text-align: left")
               .text(R[0]))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(fns.cFmt(R[1])))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(fns.cFmt(R[2])))
            .add(Q("td")
              .klass("border")
              .style("text-align: right")
              .html(fns.cFmt(R[1] + R[2])))
            .add(Q("td")
              .add(barWg(R)));}))
        .add(Q("tr")
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border"))
          .add(Q("td").klass("border")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .html(II("Sums") + "&nbsp;"))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(fns.cFmt(sumBills)))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(fns.cFmt(sumStays)))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .html(fns.cFmt(sumBills + sumStays)))
          .add(Q("td").klass("border")))
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
