import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);









export  function mk(content, withClose)  {sys.$params(arguments.length, 2);
  const wg =sys.$checkNull( Q("div")
    .style(
      "display: none;" + 
      "position: fixed;" + 
      "z-index: 1;" + 
      "padding-top: 100px;" + 
      "left: 0;" +
      "top: 0;" +
      "width: 100%;" + 
      "height: 100%;" + 
      "overflow: auto;" + 
      "background-color: rgb(0,0,0);" + 
      "background-color: rgba(0,0,0,0.4);" + 
      "text-align: center;"
    ))
  ;

  const tb =sys.$checkNull( Q("table")
    .att("align", "center")
    .style(
      "background-color: rgb(250, 250, 250);" +
      "border: 1px solid rgb(110,130,150);" +
      "padding: 4px;border-radius: 4px;"
    ));

  if (sys.asBool(withClose))
    tb.add(Q("tr")
      .add(Q("td")
        .style("width:100%;text-align:right;padding-bottom:5px")
        .add(Q("span")
          .text("["))
        .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); show({wg:wg}, false);})
          .style(
            "cursor:pointer;text-decoration: none; font-family: sans;" +
            "color: #000080;font-weight: normal;font-size:14px;"
          ).text(" X "))
        .add(Q("span")
          .text("]"))));

  tb.add(Q("tr")
    .add(Q("td")
      .add(content)));

  wg.add(tb);

   return {wg:wg};
};



export  function show(Mbox, value)  {sys.$params(arguments.length, 2);
  if (sys.asBool(value)) Mbox.wg.setStyle("display", "block");
  else Mbox.wg.setStyle("display", "none");};
