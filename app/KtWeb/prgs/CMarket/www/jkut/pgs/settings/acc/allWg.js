import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as menu from  "../../../libdm/menu.js";
import * as datePicker from  "../../../libdm/datePicker.js";
import * as msg from  "../../../wgs/msg.js";
import * as cts from  "../../../data/cts.js";
import * as ann from  "../../../data/acc/ann.js";
import * as annotationsWg from  "../../../pgs/settings/acc/annotationsWg.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk2(wg, year0, date)  {sys.$params(arguments.length, 3);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "acc/AllWg",
    rq: "idata",
    year: year0,
    date:date
  }));
  if (sys.asBool(!sys.asBool(Rp.ok)))
    msg.error(II("Some error was found.<br>See Log."), function(){sys.$params(arguments.length, 0);});
  const year =sys.$checkNull( Rp.year); 
  const Years =sys.$checkNull( Rp.years); 
  const Anns =sys.$checkNull( arr.map(Rp.anns, ann.fromJs));
  arr.sort(Anns, function(A1, A2)  {sys.$params(arguments.length, 2);  return A1.date > A2.date;}); 
  const cash =sys.$checkNull( Rp.cash); 

  

  
   function changeYear(y)  {sys.$params(arguments.length, 1); mk2(wg, y, "");};

  
   function changeLastDate(d)  {sys.$params(arguments.length, 1); mk2(wg, year, time.toStr(time.fromIso(d, "/")));};

  

  const Lopts =sys.$checkNull( []);
  arr.eachIx(Years, function(y, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(i > 0)) arr.push(Lopts, menu.separator());
    arr.push(Lopts, menu.toption(y, y, function()  {sys.$params(arguments.length, 0); changeYear(y);}));
  });
  const menuWg =sys.$checkNull( menu.mk(Lopts, [], year, false));

  const lastDateDiv =sys.$checkNull( Q("div"));

  const annsWg =sys.$checkNull( Q("div"));
  annotationsWg.mk(annsWg, Anns, []);

  wg
    .removeAll()
    .add(menuWg)
    .add(Q("div")
      .add(Q("div")
        .klass("head")
        .html(II("Annotations")))
      .add(lastDateDiv)
      .add(Q("table")
        .att("align", "center")
        .klass("frame3")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "right")
              .add(Q("tr")
                .add(Q("td")
                  .klass("rlabel")
                  .add(Q("span")
                    .html(II("Cash:"))))
                .add(Q("td")
                  .klass("number")
                  .text(math.toIso(cash, 2)))
                .add(Q("td"))))))
        .add(Q("tr")
          .add(Q("td").klass("frame")
            .add(annsWg)))))
  ;

  if (sys.asBool(sys.asBool(sys.$eq(year , Years[0])) && sys.asBool(Anns))) {
    const dp =sys.$checkNull( datePicker.mk(
      sys.$eq(i18n.getLang() , "es"),
      time.fromStr(Anns[0].date),
      function(s)  {sys.$params(arguments.length, 1);}
    ));
    const input =sys.$checkNull( Q("input")
      .att("type", "text")
      .style("text-align:center;width:166px"))
    ;
    const button =sys.$checkNull( Q("button")
      .text(II("Change"))
      .on("click", function(e)  {sys.$params(arguments.length, 1); changeLastDate(input.getValue());}))
    ;
    lastDateDiv
      .removeAll()
      .style("text-align: center")
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("width:120px;text-align:right")
            .text(II("Up to") + ": "))
          .add(Q("td")
            .add(datePicker.mkText(dp, input)))
          .add(Q("td")
            .style("width:120px;text-align:left")
            .add(button))))
    ;
  }
};


export  async  function mk(wg)  {sys.$params(arguments.length, 1); mk2(wg, "", "");};
